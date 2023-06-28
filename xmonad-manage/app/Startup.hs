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

data Startup = Startup
  { startInstall :: !FilePath
  , startRun :: !FilePath
  , startEnv :: !(M.Map T.Text T.Text)
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
      logger "Custom installation for startup..."
      traverse_ setToExecutable [startInstall, startRun]
      callProcess startInstall []

runStartup :: ManageEnv -> Startup -> IO ()
runStartup mEnv Startup{..} = do
  for_ (M.toList startEnv) $ \(key, val) -> do
    formatted <- expanded (parsed val)
    setEnv (T.unpack key) (T.unpack formatted)
  startX11 mEnv -- X11 is handled here
  callProcess startRun [] -- Specific startups
  where
    -- FIXME Proper shell variable expansion support
    pref = T.pack "${"
    suff = T.pack "}"
    parsed val = Right first : identified
      where
        first : others = T.splitOn pref val
        identified = do
          part <- others
          let (var, postIncl) = T.breakOn suff part
          let post = T.replace suff T.empty postIncl
          [Left var, Right post]
    expand = \case
      Left var -> T.pack <$> getEnv (T.unpack var)
      Right str -> pure str
    expanded ptext = mconcat <$> traverse expand ptext

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
