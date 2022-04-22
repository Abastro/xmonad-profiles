module Profile where

import Checked
import Data.Text qualified as T
import Control.Exception
import Text.Printf
import System.FilePath
import System.Directory
import Control.Monad
--import Text.Parsec

-- | Profile Config
data ProfileCfg = ProfileCfg
  { profileID :: !ID,
    profileName :: !T.Text,
    installScript :: !(Maybe FilePath)
  }
  deriving (Read, Show)

-- | Profile. Requires the config path to exist.
data Profile = Profile
  { profCfg :: !ProfileCfg,
    cfgDir, dataDir, cacheDir, logDir :: !FilePath,
    starter :: !FilePath
  }

data ProfileError
  = ProfileNotFound ID
  | ProfileWrongFormat ID String
  deriving (Show)

instance Exception ProfileError

-- | Gets a profile, or gives Nothing if not found.
getProfile :: FilePath -> ID -> IO Profile
getProfile parent profID = do
  doesDirectoryExist cfgDir >>= (`unless` throwIO (ProfileNotFound profID))
  cfgStr <- readFile (cfgDir </> "profile.cfg")
  profCfg <- case reads @ProfileCfg cfgStr of
    [(cfg, "")] -> pure cfg -- TODO Use proper parser combinators
    remaining -> throwIO (ProfileWrongFormat profID $ errMsg $ show remaining)
  pure (Profile {profCfg, cfgDir, dataDir, cacheDir, logDir, starter})
  where
    errMsg = printf "left while parsing: %s"
    dataDir = parent </> "data" </> idStr profID
    cfgDir = parent </> idStr profID
    cacheDir = parent </> "cache" </> idStr profID
    logDir = parent </> "logs" </> idStr profID
    starter = parent </> "start.sh"
