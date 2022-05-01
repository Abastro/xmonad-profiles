module Profile where

import Checked
import Config
import Control.Exception hiding (try)
import Control.Monad
import Data.Text qualified as T
import Data.Text.IO qualified as T
import System.Directory
import System.FilePath
import Text.Printf
import System.Info (arch, os)

-- | Profile Config
data ProfileCfg = ProfileCfg
  { profileID :: !ID,
    profileName :: !T.Text,
    installScript :: !(Maybe FilePath),
    buildOnStart :: Bool
  }
  deriving (Read, Show)

-- | Profile. Requires the config path to exist.
data Profile = Profile
  { profID :: !ID,
    profName :: !T.Text,
    profInstall :: !(Maybe Executable),
    xmonadExe :: !Executable,
    cfgDir, dataDir, cacheDir, logDir :: !FilePath
  }

data ProfileError
  = ProfileNotFound (Either FilePath ID)
  | ProfileIOError FilePath IOError
  | ProfileWrongFormat String
  deriving (Show)

instance Exception ProfileError

parseCfg :: FilePath -> T.Text -> Either ParseError ProfileCfg
parseCfg =
  parse $
    completeP . recordP "ProfileCfg" $
      ProfileCfg
        <$> fieldP "profileID" identP
        <*> (commaP *> fieldP "profileName" textP)
        <*> (commaP *> fieldP "installScript" (maybeP pathP))
        <*> (commaP *> fieldP "buildOnStart" boolP)

-- | Gets a profile from specified path.
getProfileFromPath :: FilePath -> FilePath -> IO Profile
getProfileFromPath project cfgDir = do
  doesDirectoryExist cfgDir >>= (`unless` throwIO (ProfileNotFound $ Left cfgDir))
  cfgTxt <- catch @IOError (T.readFile cfgLoc) $ throwIO . ProfileIOError cfgDir
  ProfileCfg {..} <- case parseCfg cfgLoc cfgTxt of
    Left err -> throwIO (ProfileWrongFormat $ show err)
    Right cfg -> pure cfg

  let (profID, profName) = (profileID, profileName)
      installPath = (</>) cfgDir <$> installScript
      [dataDir, cacheDir, logDir] = locFor profileID <$> ["data", "cache", "logs"]
      customPath = cacheDir </> printf "xmonad-%s-%s" arch os
  
  profInstall <- traverse setToExecutable installPath
  customExe <- if buildOnStart then pure Nothing else mayExecutable customPath
  xmonadExe <- maybe (getExecutable "xmonad") pure customExe
  pure (Profile {..})
  where
    cfgLoc = cfgDir </> "profile.cfg"
    locFor ident str = project </> str </> idStr ident
