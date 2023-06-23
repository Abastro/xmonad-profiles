module Startup where

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

-- | Gets startup from path
getStartup :: FilePath -> IO Startup
getStartup startupDir = do
  Startup{..} <- readYAMLFile userError (startupDir </> "startup.yaml")
  pure Startup{startInstall = startupDir </> startInstall, startRun = startupDir </> startRun, ..}

installStartup :: ManageEnv -> PkgDatabase -> Distro -> Startup -> IO ()
installStartup mEnv pkgDb distro Startup{..} = do
  [reqs, _] <- traverse setToExecutable [startInstall, startRun]
  installX11 mEnv pkgDb distro
  installPackages mEnv pkgDb distro dependencies
  callExe reqs []

runStartup :: ManageEnv -> Startup -> IO ()
runStartup mEnv Startup{..} = do
  for_ (M.toList startEnv) $ \(key, val) -> setEnv (T.unpack key) (T.unpack val)
  startX11 mEnv -- X11 is handled here
  callProcess startRun [] -- Specific startups

installX11 :: ManageEnv -> PkgDatabase -> Distro -> IO ()
installX11 mEnv@ManageEnv{..} pkgDb distro = do
  homeDir <- getHomeDirectory
  installPackages mEnv pkgDb distro [AsPackage (T.pack "xsettingsd")]
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
