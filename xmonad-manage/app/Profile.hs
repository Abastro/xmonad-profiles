module Profile where

import Common
import Control.Exception
import Control.Monad
import Data.Foldable
import Data.Text qualified as T
import Data.YAML
import Manages
import System.Directory
import System.Environment
import System.FilePath
import System.IO
import System.Info (arch, os)
import System.Process
import Text.Printf

-- | Profile Properties
data ProfileProps = ProfileProps
  { profileName :: !T.Text,
    profileDetails :: !T.Text
  }
  deriving (Show)

-- | Profile Config
data ProfileCfg = ProfileCfg
  { profileID :: !ID,
    profileProps :: !ProfileProps,
    installScript :: !(Maybe FilePath),
    buildOnStart :: Bool
  }
  deriving (Show)

instance FromYAML ProfileCfg where
  parseYAML = withMap "profile" $ \m ->
    ProfileCfg
      <$> m .: T.pack "ID"
      <*> (ProfileProps <$> m .: T.pack "name" <*> m .: T.pack "details")
      <*> (fmap T.unpack <$> m .:? T.pack "install")
      <*> m .: T.pack "buildOnStart"

-- | Profile. Requires the config path to exist.
data Profile = Profile
  { profID :: !ID,
    profProps :: !ProfileProps,
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

-- | Gets a profile from specified path.
getProfileFromPath :: FilePath -> FilePath -> IO Profile
getProfileFromPath project cfgDir = do
  doesDirectoryExist cfgDir >>= (`unless` throwIO (ProfileNotFound $ Left cfgDir))
  ProfileCfg {..} <- readYAMLFile (ProfileIOError cfgDir) ProfileWrongFormat (cfgDir </> "profile.yaml")

  let installPath = (</>) cfgDir <$> installScript
      [dataDir, cacheDir, logDir] = locFor profileID <$> ["data", "cache", "logs"]
      customPath = cacheDir </> printf "xmonad-%s-%s" arch os

  profInstall <- traverse setToExecutable installPath
  customExe <- if buildOnStart then pure Nothing else mayExecutable customPath
  xmonadExe <- maybe (getExecutable "xmonad") pure customExe
  pure (Profile {profID = profileID, profProps = profileProps, ..})
  where
    locFor ident str = project </> str </> idStr ident

-- | Installs a profile - first argument is sudo.
installProfile :: ManageEnv -> Profile -> Executable -> IO ()
installProfile mEnv profile@Profile {..} sudo = do
  writeFile startPath startScript
  _ <- setToExecutable startPath

  writeFile runnerPath (runner profProps)
  callExe sudo ["ln", "-sf", runnerPath, runnerLinked]

  traverse_ (`callExe` []) profInstall
  runProfile mEnv profile True ["--recompile"]
  where
    runnerLinked = "/usr" </> "share" </> "xsessions" </> idStr profID <.> "desktop"
    runnerPath = dataDir </> "run" <.> "desktop"
    runner ProfileProps {..} =
      unlines
        [ printf "[Desktop Entry]",
          printf "Encoding=UTF-8",
          printf "Name=%s" profileName,
          printf "Comment=%s" profileDetails,
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
