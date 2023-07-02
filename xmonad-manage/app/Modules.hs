module Modules (
  ModuleSaved (..),
  ModuleMode (..),
  mkVarModS,
  combinedModule,
  loadModule,
  x11ModuleAt,
) where

import Common
import Component
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

combinedModule :: FilePath -> ModuleSaved -> IO (Component ModuleMode)
combinedModule home (ModuleSaved modules) = do
  combined <- mconcat <$> traverse loadModule modules
  pure (x11ModuleAt home <> combined)

data ModuleType = Basis | Compositor | Display | Input | PolicyKit | Keyring
  deriving (Show)

data ModuleMode = Start
  deriving (Show, Enum, Bounded)

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

loadModule :: FilePath -> IO (Component ModuleMode)
loadModule moduleDir = moduleOf <$> readYAMLFile userError (moduleDir </> "module.yaml")
  where
    scriptFor path = moduleDir </> path
    moduleOf ModuleCfgOf{..} = deps <> setupEnv <> scripts
      where
        deps = ofDependencies dependencies
        scripts = fromScript executeScript (scriptFor <$> install) Nothing (\Start -> scriptFor run)
        setupEnv = ofHandle (setupEnvivonment moduleDir environment)

executeScript :: ManageEnv -> FilePath -> Context ModuleMode -> IO ()
executeScript ManageEnv{..} script = \case
  CustomInstall -> do
    logger "Custom installation using %s..." script
    callProcess script []
  CustomRemove -> pure ()
  InvokeOn Start -> do
    logger "Setting up using %s..." script
    callProcess script []
    logger "Setup complete."

setupEnvivonment :: FilePath -> M.Map T.Text ShellString -> ManageEnv -> Context ModuleMode -> IO ()
setupEnvivonment moduleDir environment ManageEnv{..} = \case
  CustomInstall -> do
    logger "[%s] Checking shell expansions..." moduleDir
    forInEnv (logger "[Env] %s=%s")
  CustomRemove -> pure ()
  InvokeOn Start -> do
    logger "[%s] Setting up..." moduleDir
    forInEnv setEnv
  where
    forInEnv act = for_ (M.toList environment) $ \(key, str) -> do
      formatted <- shellExpand str
      act (T.unpack key) (T.unpack formatted)

-- Includes XMonad.
x11ModuleAt :: FilePath -> Component ModuleMode
x11ModuleAt home = deps <> xsettings <> xRandr <> xResources <> xSetRoot
  where
    deps = ofDependencies [AsPackage (T.pack "libxss"), AsPackage (T.pack "xmonad"), AsPackage (T.pack "xsettingsd")]
    -- X settings daemon to provide settings
    xsettings = ofHandle $ \ManageEnv{..} -> \case
      CustomInstall -> do
        logger "Copying xsettings.conf..."
        copyFile (envPath </> "database" </> "xsettingsd.conf") (home </> ".config" </> "xsettingsd" </> "xsettingsd.conf")
      CustomRemove -> logger "You may remove xsettings.conf in ~/.config directory."
      InvokeOn Start -> do
        _ <- spawnProcess "xsettingsd" []
        pure ()
    -- Load X resources
    xResources = ofHandle $ \ManageEnv{..} -> \case
      CustomInstall -> do
        logger "Copying .Xresources..."
        copyFile (envPath </> "database" </> ".Xresources") (home </> ".Xresources")
      CustomRemove -> pure ()
      InvokeOn Start -> do
        callProcess "xrdb" ["-merge", home </> ".Xresources"]
    -- Monitor settings use xrandr
    xRandr = ofHandle $ \_ -> \case
      InvokeOn Start -> callProcess "xrandr" []
      _ -> pure ()
    -- Set root cursor - maybe we could set other settings as well.
    xSetRoot = ofHandle $ \_ -> \case
      InvokeOn Start -> callProcess "xsetroot" ["-cursor_name", "left_ptr"]
      _ -> pure ()
