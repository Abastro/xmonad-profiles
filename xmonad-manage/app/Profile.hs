module Profile where

import Checked
import Config
import Control.Exception hiding (try)
import Control.Monad
import Data.Text qualified as T
import Data.Text.IO qualified as T
import System.Directory
import System.FilePath
import Text.Parsec (ParseError, Parsec, parse)


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
  = ProfileNotFound (Either FilePath ID)
  | ProfileIOError FilePath IOError
  | ProfileWrongFormat String
  deriving (Show)

instance Exception ProfileError

parseCfg :: FilePath -> T.Text -> Either ParseError ProfileCfg
parseCfg cfgPath = parse parserCfg cfgPath
  where
    parserCfg :: Parsec T.Text () ProfileCfg
    parserCfg =
      completeP . recordP "ProfileCfg" $
        ( ProfileCfg <$> fieldP "profileID" identP
            <*> (commaP *> fieldP "profileName" textP)
            <*> (commaP *> fieldP "installScript" (maybeP pathP))
        )

-- | Gets a profile from specified path.
getProfileFromPath :: FilePath -> FilePath -> IO Profile
getProfileFromPath project cfgDir = do
  doesDirectoryExist cfgDir >>= (`unless` throwIO (ProfileNotFound $ Left cfgDir))
  cfgTxt <- catch @IOError (T.readFile cfgLoc) $ throwIO . ProfileIOError cfgDir

  profCfg@ProfileCfg {profileID} <-
    case parseCfg cfgLoc cfgTxt of
      Left err -> throwIO (ProfileWrongFormat $ show err)
      Right cfg -> pure cfg

  let [dataDir, cacheDir, logDir] = locFor profileID <$> ["data", "cache", "logs"]
  pure (Profile {profCfg, cfgDir, dataDir, cacheDir, logDir, starter})
  where
    cfgLoc = cfgDir </> "profile.cfg"
    locFor ident str = project </> str </> idStr ident
    starter = project </> "start.sh"
