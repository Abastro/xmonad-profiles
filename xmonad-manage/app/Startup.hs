module Startup where

import Common
import Data.Foldable
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.YAML
import Packages
import System.Environment
import System.FilePath
import System.Process
import Manages
import System.Directory

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
installStartup _ pkgDb distro Startup{..} = do
  [reqs, _] <- traverse setToExecutable [startInstall, startRun]
  installPackages pkgDb distro dependencies
  callExe reqs []

runStartup :: ManageEnv -> Startup -> IO ()
runStartup ManageEnv{..} Startup{..} = do
  for_ (M.toList startEnv) $ \(key, val) -> setEnv (T.unpack key) (T.unpack val)  
  -- X11 is handled here
  startX11
  -- Specific startups
  callProcess startRun []
  where
    startX11 = do
      homeDir <- getHomeDirectory
      -- Monitor settings use xrandr
      callProcess "xrandr" []
      -- Copy X resources
      copyFile (envPath </> "database" </> ".Xresources") (homeDir </> ".Xresources")
      -- Load X resources
      callProcess "xrdb" ["-merge", "~/.Xresources"]
      -- Set root cursor
      callProcess "xsetroot" ["-cursor_name", "left_ptr"]
