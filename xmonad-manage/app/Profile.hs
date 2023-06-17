module Profile (
  ProfileProps (..),
  Profile (..),
  ProfileError (..),
  withProfile,
  installProfile,
  removeProfile,
  buildProfile,
  runProfile,
) where

import Common
import Control.Exception
import Data.Foldable
import Data.Text qualified as T
import Data.YAML
import Manages
import System.Directory
import System.Environment
import System.FilePath
import System.Info (arch, os)
import Text.Printf
import Packages

-- | Profile Properties
data ProfileProps = ProfileProps
  { profileName :: !T.Text
  , profileDetails :: !T.Text
  }
  deriving (Show)

-- | Profile Config
data ProfileCfg = ProfileCfg
  { profileID :: !ID
  , profileProps :: !ProfileProps
  , installScript :: !(Maybe FilePath)
  , buildScript, runScript :: !FilePath
  , dependencies :: [Package]
  }
  deriving (Show)

instance FromYAML ProfileCfg where
  parseYAML :: Node Pos -> Parser ProfileCfg
  parseYAML = withMap "profile" $ \m ->
    ProfileCfg
      <$> m
      .: T.pack "ID"
      <*> (ProfileProps <$> m .: T.pack "name" <*> m .: T.pack "details")
      <*> (fmap T.unpack <$> m .:? T.pack "install")
      <*> (T.unpack <$> m .: T.pack "build")
      <*> (T.unpack <$> m .: T.pack "run")
      <*> (m .: T.pack "dependencies")

-- | Profile. Requires the config path to exist.
data Profile = Profile
  { profID :: !ID
  , profProps :: !ProfileProps
  , profInstall :: !(Maybe Executable)
  , profBuild, profRun :: !Executable
  , cfgDir, dataDir, cacheDir, logDir :: !FilePath
  , profDeps :: [Package]
  }

data ProfileError
  = ProfileNotFound ID
  | ProfileIOError FilePath IOError
  | ProfileWrongFormat String
  deriving (Show)

instance Exception ProfileError

withProfile :: ManageEnv -> FilePath -> (Profile -> IO a) -> IO a
withProfile ManageEnv{..} cfgDir withProf = handle @IOError onIOErr $ do
  ProfileCfg{..} <- readYAMLFile ProfileWrongFormat (cfgDir </> "profile.yaml")
  let [dataDir, cacheDir, logDir] = locFor profileID <$> ["data", "cache", "logs"]
  profInstall <- traverse setToExecutable $ (cfgDir </>) <$> installScript
  [profBuild, profRun] <- traverse setToExecutable $ (cfgDir </>) <$> [buildScript, runScript]
  do
    setEnv "ENV_ARCH" arch >> setEnv "ENV_OS" os
    setEnv "XMONAD_DATA_DIR" dataDir
    setEnv "XMONAD_CONFIG_DIR" cfgDir
    setEnv "XMONAD_CACHE_DIR" cacheDir
    setEnv "XMONAD_LOG_DIR" logDir
  withProf $ Profile{profID = profileID, profProps = profileProps, profDeps = dependencies, ..}
 where
  onIOErr = throwIO . ProfileIOError cfgDir
  locFor ident str = envPath </> str </> idStr ident

-- | Path of the runner file.
runnerLinkPath profID = "/" </> "usr" </> "share" </> "xsessions" </> idStr profID <.> "desktop"

installProfile :: ManageEnv -> PkgDatabase -> Distro -> Profile -> Executable -> IO ()
installProfile mEnv@ManageEnv{logger} pkgDb distro profile@Profile{..} sudo = do
  -- Prepares directories
  traverse_ (createDirectoryIfMissing True) [dataDir, cacheDir, logDir]

  -- Running setup
  writeFile startPath startScript
  _ <- setToExecutable startPath
  writeFile runnerPath (runner profProps)
  -- Instead of linking, we copy the runner. Fixes issues with SDDM.
  callExe sudo ["cp", "-T", runnerPath, runnerLinkPath profID]

  logger "Installing dependencies"
  installPackages mEnv pkgDb distro profDeps

  logger "Install using %s" (show profInstall)
  traverse_ (`callExe` []) profInstall
  buildProfile mEnv profile
 where
  runnerPath = dataDir </> "run" <.> "desktop"
  runner ProfileProps{..} =
    unlines
      [ printf "[Desktop Entry]"
      , printf "Encoding=UTF-8"
      , printf "Type=XSession"
      , printf "Name=%s" profileName
      , printf "Comment=%s" profileDetails
      , printf "Exec=%s" startPath
      ]

  startPath = dataDir </> "starter.sh"
  startScript =
    unlines
      [ printf "#!/bin/sh"
      , printf "export PATH=$HOME/.cabal/bin:$HOME/.ghcup/bin:$PATH"
      , printf
          "exec xmonad-manage run %s > %s 2> %s"
          (idStr profID)
          (show $ logDir </> "start.log")
          (show $ logDir </> "start.err")
      ]

removeProfile :: ManageEnv -> Profile -> Executable -> IO ()
removeProfile ManageEnv{logger} Profile{..} sudo = do
  -- NOTE: cfgDir is not removed, as it is the source of the entire installation
  logger "Removing profile-specific directories"
  traverse_ removePathForcibly [dataDir, cacheDir, logDir]
  logger "Removing xsession runner, asking sudo"
  callExe sudo ["rm", "-f", runnerLinkPath profID]

buildProfile :: ManageEnv -> Profile -> IO ()
buildProfile ManageEnv{logger} Profile{cfgDir, profBuild} = do
  logger "Build using %s" (show profBuild)
  withCurrentDirectory cfgDir $ callExe profBuild []

runProfile :: ManageEnv -> Profile -> IO ()
runProfile ManageEnv{logger} Profile{profRun} = do
  logger "Run using %s" (show profRun)
  callExe profRun []
