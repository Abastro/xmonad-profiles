module Startup where

import Common
import Data.Foldable
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.YAML
import System.Environment
import System.FilePath
import System.Process

data Startup = Startup
  { startInstall :: !FilePath,
    startRun :: !FilePath,
    startEnv :: !(M.Map T.Text T.Text)
  }
  deriving (Show)

instance FromYAML Startup where
  parseYAML = withMap "startup" $ \m ->
    Startup
      <$> (T.unpack <$> m .: T.pack "install")
      <*> (T.unpack <$> m .: T.pack "run")
      <*> m .: T.pack "environment"

-- | Gets startup from path
getStartup :: FilePath -> IO Startup
getStartup startupDir = do
  Startup {..} <- readYAMLFile userError (startupDir </> "startup.yaml")
  pure Startup {startInstall = startupDir </> startInstall, startRun = startupDir </> startRun, startEnv}

installStartup :: Startup -> IO ()
installStartup Startup {..} = do
  [reqs, _] <- traverse setToExecutable [startInstall, startRun]
  callExe reqs []

runStartup :: Startup -> IO ()
runStartup Startup {..} = do
  for_ (M.toList startEnv) $ \(key, val) -> setEnv (T.unpack key) (T.unpack val)
  callProcess startRun []
