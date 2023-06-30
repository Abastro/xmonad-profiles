module Startup (
  Startup (..),
  withStartup,
  startupReqs,
  runStartup,
  x11Reqs,
) where

import Common
import Data.Foldable
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.YAML
import Manages
import Packages
import System.Directory
import System.Environment
import System.FilePath
import System.Process
import Text.Parsec qualified as P

data Startup = Startup
  { startInstall :: !FilePath
  , startRun :: !FilePath
  , startEnv :: !(M.Map T.Text ShellString)
  , dependencies :: [Package] -- Not worth pulling in vector just for this
  }
  deriving (Show)

instance FromYAML Startup where
  parseYAML :: Node Pos -> Parser Startup
  parseYAML = withMap "startup" $ \m ->
    Startup
      <$> (T.unpack <$> m .: T.pack "install")
      <*> (T.unpack <$> m .: T.pack "run")
      <*> (m .: T.pack "environment")
      <*> (m .: T.pack "dependencies")

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
      -- Forgot how to do without the guard
      nominal = P.try $ T.pack <$> P.many1 (P.satisfy (/= '}'))
      shellVar = P.between (P.string "${") (P.string "}") nominal

shellExpand :: ShellString -> IO T.Text
shellExpand (MkShellStr strs) = mconcat <$> traverse expand strs
  where
    expand = \case
      Str str -> pure str
      Var var -> T.pack <$> getEnv (T.unpack var)

withStartup :: FilePath -> (Startup -> IO a) -> IO a
withStartup startupDir withStart = do
  Startup{..} <- readYAMLFile userError (startupDir </> "startup.yaml")
  withStart Startup{startInstall = startupDir </> startInstall, startRun = startupDir </> startRun, ..}

startupReqs :: Startup -> Requirement
startupReqs Startup{..} =
  MkRequirement
    { customInstall
    , customRemove = mempty
    , requiredDeps = dependencies
    }
  where
    customInstall ManageEnv{..} = do
      logger "Checking shell expansions..."
      for_ (M.toList startEnv) $ \(key, str) -> do
        formatted <- shellExpand str
        logger "[Env] %s=%s" key formatted

      logger "Custom installation for startup..."
      traverse_ setToExecutable [startInstall, startRun]
      callProcess startInstall []

runStartup :: ManageEnv -> Startup -> IO ()
runStartup mEnv Startup{..} = do
  for_ (M.toList startEnv) $ \(key, str) -> do
    formatted <- shellExpand str
    setEnv (T.unpack key) (T.unpack formatted)
  startX11 mEnv -- X11 is handled here
  callProcess startRun [] -- Specific startups

x11Reqs :: Requirement
x11Reqs =
  MkRequirement
    { customInstall
    , customRemove = mempty
    , requiredDeps = [AsPackage (T.pack "xsettingsd")]
    }
  where
    customInstall ManageEnv{..} = do
      homeDir <- getHomeDirectory
      logger "Copying .Xresources and xsettings.conf..."
      -- Copy X settings and resources
      copyFile (envPath </> "database" </> ".Xresources") (homeDir </> ".Xresources")
      copyFile (envPath </> "database" </> "xsettingsd.conf") (homeDir </> ".config" </> "xsettingsd" </> "xsettingsd.conf")

startX11 :: ManageEnv -> IO ()
startX11 ManageEnv{} = do
  homeDir <- getHomeDirectory
  -- X settings daemon to provide settings value
  _ <- spawnProcess "xsettingsd" []
  -- Monitor settings use xrandr
  callProcess "xrandr" []
  -- Load X resources
  callProcess "xrdb" ["-merge", homeDir </> ".Xresources"]
  -- Set root cursor
  callProcess "xsetroot" ["-cursor_name", "left_ptr"]
