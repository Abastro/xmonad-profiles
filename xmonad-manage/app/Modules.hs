module Modules (
  Module (..),
  ModuleSaved (..),
  mkVarModS,
  combinedModule,
  loadModule,
  x11ModuleAt,
) where

import Common
import Data.Foldable
import Data.Map.Strict qualified as M
import Data.Serialize (Serialize)
import Data.StateVar
import Data.Text qualified as T
import Data.YAML
import GHC.Generics
import Manages
import Packages
import System.Directory
import System.Environment
import System.FilePath
import System.Process
import Text.Parsec qualified as P

newtype ModuleSaved = ModuleSaved [FilePath]
  deriving (Show, Generic)
instance Serialize ModuleSaved

mkVarModS :: ManageEnv -> (StateVar ModuleSaved, IO ())
mkVarModS ManageEnv{..} = dataVar "xmonad-manage" "active-modules" $ do
  logger "Cannot load active modules, restored to default. Please run setup."
  pure . ModuleSaved $ ((envPath </> "modules") </>) <$> defModules
  where
    defModules = ["compositor-picom", "display-lightdm", "policykit-gnome", "input-ibus", "keyring-gnome"]

combinedModule :: FilePath -> ModuleSaved -> IO Module
combinedModule home (ModuleSaved modules) = do
  combined <- mconcat <$> traverse loadModule modules
  pure (x11ModuleAt home <> combined)

-- | Representation of a module, which is invoked on startup.
data Module = MkModule
  { requirement :: Requirement
  , onStartup :: ManageEnv -> IO ()
  }

instance Semigroup Module where
  (<>) :: Module -> Module -> Module
  MkModule req start <> MkModule req' start' = MkModule (req <> req') (start <> start')
instance Monoid Module where
  mempty :: Module
  mempty = MkModule mempty mempty

data ModuleCfg = ModuleCfgOf
  { install :: !(Maybe FilePath)
  , run :: !FilePath
  , environment :: !(M.Map T.Text ShellString)
  , dependencies :: [Package] -- Not worth pulling in vector just for this
  }
  deriving (Show)

instance FromYAML ModuleCfg where
  parseYAML :: Node Pos -> Parser ModuleCfg
  parseYAML = withMap "module" $ \m ->
    ModuleCfgOf
      <$> (fmap T.unpack <$> m .:! T.pack "install")
      <*> (T.unpack <$> m .: T.pack "run")
      <*> (m .:? T.pack "environment" .!= M.empty)
      <*> (m .:? T.pack "dependencies" .!= [])

newtype ShellString = MkShellStr [ShellStrElem]
  deriving (Show)
data ShellStrElem = Str !T.Text | Var !T.Text
  deriving (Show)

instance FromYAML ShellString where
  parseYAML :: Node Pos -> Parser ShellString
  parseYAML = withStr "shell-string" $ \txt -> case P.parse shellStr "shell" txt of
    Left err -> fail (show err)
    Right res -> pure res
    where
      shellStr = MkShellStr <$> P.many elem <* P.eof
      elem = Var <$> shellVar P.<|> Str <$> nominal
      nominal = P.try $ T.pack <$> P.many1 (P.satisfy (/= '}'))
      shellVar = P.between (P.string "${") (P.string "}") nominal

shellExpand :: ShellString -> IO T.Text
shellExpand (MkShellStr strs) = mconcat <$> traverse expand strs
  where
    expand = \case
      Str str -> pure str
      Var var -> T.pack <$> getEnv (T.unpack var)

loadModule :: FilePath -> IO Module
loadModule moduleDir = do
  cfg <- readYAMLFile userError (moduleDir </> "module.yaml")
  pure MkModule{requirement = moduleReqs moduleDir cfg, onStartup = moduleOnStart moduleDir cfg}

moduleReqs :: FilePath -> ModuleCfg -> Requirement
moduleReqs moduleDir ModuleCfgOf{..} =
  MkRequirement
    { customInstall
    , customRemove = mempty
    , requiredDeps = dependencies
    }
  where
    customInstall ManageEnv{..} = do
      logger "[%s] Checking shell expansions..." moduleDir
      for_ (M.toList environment) $ \(key, str) -> do
        formatted <- shellExpand str
        logger "[Env] %s=%s" key formatted

      logger "[%s] Custom installation for module..." moduleDir
      for_ install $ \inst -> do
        setToExecutable (moduleDir </> inst)
        callProcess (moduleDir </> inst) []
      setToExecutable (moduleDir </> run)
      logger "[%s] Done." moduleDir

moduleOnStart :: FilePath -> ModuleCfg -> ManageEnv -> IO ()
moduleOnStart moduleDir ModuleCfgOf{..} ManageEnv{..} = do
  -- Environment setup
  logger "[%s] Setting up..." moduleDir
  for_ (M.toList environment) $ \(key, str) -> do
    formatted <- shellExpand str
    setEnv (T.unpack key) (T.unpack formatted)
  callProcess (moduleDir </> run) []
  logger "[%s] Setup complete." moduleDir

-- Includes XMonad.
x11ModuleAt :: FilePath -> Module
x11ModuleAt home = do
  MkModule{..}
  where
    requirement =
      MkRequirement
        { customInstall
        , customRemove = mempty
        , -- XMonad requires libxss.
          requiredDeps = [AsPackage (T.pack "libxss"), AsPackage (T.pack "xmonad"), AsPackage (T.pack "xsettingsd")]
        }
    customInstall ManageEnv{..} = do
      logger "Copying .Xresources and xsettings.conf..."
      -- Copy X settings and resources
      copyFile (envPath </> "database" </> ".Xresources") (home </> ".Xresources")
      copyFile (envPath </> "database" </> "xsettingsd.conf") (home </> ".config" </> "xsettingsd" </> "xsettingsd.conf")

    onStartup ManageEnv{} = do
      -- X settings daemon to provide settings value
      _ <- spawnProcess "xsettingsd" []
      -- Monitor settings use xrandr
      callProcess "xrandr" []
      -- Load X resources
      callProcess "xrdb" ["-merge", home </> ".Xresources"]
      -- Set root cursor
      callProcess "xsetroot" ["-cursor_name", "left_ptr"]
