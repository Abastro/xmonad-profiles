module Profile where

import Common
import Config
import Control.Exception hiding (try)
import Control.Monad
import Data.Text qualified as T
import Data.Text.IO qualified as T
import System.Directory
import System.FilePath
import System.Info (arch, os)
import Text.Printf
import Manages
import System.Process
import System.Environment
import System.IO
import Data.Foldable

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

-- | Installs a profile - first argument is sudo.
installProfile :: ManageEnv -> Profile -> Executable -> IO ()
installProfile mEnv profile@Profile {..} sudo = do
  writeFile startPath startScript
  _ <- setToExecutable startPath

  writeFile runnerPath runner
  callExe sudo ["ln", "-sf", runnerPath, runnerLinked]

  traverse_ (`callExe` []) profInstall
  runProfile mEnv profile True ["--recompile"]
  where
    runnerLinked = "/usr" </> "share" </> "xsessions" </> idStr profID <.> "desktop"
    runnerPath = dataDir </> "run" <.> "desktop"
    runner =
      unlines
        [ printf "[Desktop Entry]",
          printf "Encoding=UTF-8",
          printf "Name=%s" profName,
          printf "Comment=Xmonad profile %s" (idStr profID),
          printf "Exec=%s" startPath,
          printf "Type=XSession"
        ]

    startPath = dataDir </> "starter.sh"
    startScript =
      unlines
        [ printf "#!/bin/sh",
          printf "export PATH=$HOME/.cabal/bin:$HOME/.ghcup/bin:$PATH",
          printf
            "exec xmonad-manage run %s > %s 2> %s"
            (idStr profID)
            (show $ logDir </> "start.log")
            (show $ logDir </> "start.err")
        ]

-- | Run profile xmoand instance with given options.
runProfile :: ManageEnv -> Profile -> Bool -> [String] -> IO ()
runProfile ManageEnv {logger} Profile {xmonadExe, dataDir, cfgDir, cacheDir, logDir} untilEnd opts = do
  setEnv "XMONAD_DATA_DIR" dataDir
  setEnv "XMONAD_CONFIG_DIR" cfgDir
  setEnv "XMONAD_CACHE_DIR" cacheDir
  logger "Running xmonad through %s" (show xmonadExe)
  if untilEnd
    then callExe xmonadExe opts
    else do
      [logs, errors] <- traverse openLog ["xmonad.log", "xmonad.err"]
      withCreateProcess (exeToProc xmonadExe opts) {std_out = UseHandle logs, std_err = UseHandle errors} $
        \_ _ _ ph -> () <$ waitForProcess ph
  where
    openLog name = openFile (logDir </> name) WriteMode
