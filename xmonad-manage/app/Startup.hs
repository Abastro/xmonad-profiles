module Startup where

import Config
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Text.IO qualified as T
import System.FilePath

data Startup = Startup
  { startInstall :: !FilePath,
    startRun :: !FilePath,
    startEnv :: !(M.Map String T.Text)
  }

parseStart :: FilePath -> FilePath -> T.Text -> Either ParseError Startup
parseStart parent = parse startP
  where
    startP =
      completeP . recordP "Startup" $
        Startup
          <$> fieldP "startInstall" ((</>) parent <$> pathP)
          <*> (commaP *> fieldP "startRun" ((</>) parent <$> pathP))
          <*> (commaP *> fieldP "startEnv" (mapP textP))

-- | Gets startup from path
getStartup :: FilePath -> IO Startup
getStartup startupDir = do
  startupTxt <- T.readFile startupLoc
  case parseStart startupDir startupLoc startupTxt of
    Left err -> fail (show err)
    Right st -> pure st
  where
    startupLoc = startupDir </> "startup.cfg"
