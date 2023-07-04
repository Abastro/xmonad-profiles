module Profile (
  ProfileProps (..),
  ProfileCfg (..),
  ProfileError (..),
  ProfileMode (..),
  readProfileCfg,
  loadProfile,
) where

import Common
import Component
import Control.Exception
import Data.Foldable
import Data.Text qualified as T
import Data.YAML
import Manages
import Packages
import System.Directory
import System.Environment
import System.FilePath
import System.Info (arch, os)
import System.Process
import Text.Printf

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
      <$> (m .: T.pack "ID")
      <*> (ProfileProps <$> m .: T.pack "name" <*> m .: T.pack "details")
      <*> (fmap T.unpack <$> m .:? T.pack "install")
      <*> (T.unpack <$> m .: T.pack "build")
      <*> (T.unpack <$> m .: T.pack "run")
      <*> (m .:? T.pack "dependencies" .!= [])

data ProfileError
  = ProfileNotFound ID
  | -- | IO Error while "loading" the profile.
    ProfileIOError FilePath IOError
  | ProfileWrongFormat String
  deriving (Show)

instance Exception ProfileError

readProfileCfg :: FilePath -> IO ProfileCfg
readProfileCfg cfgDir = wrapIOError cfgDir $ readYAMLFile ProfileWrongFormat (cfgDir </> "profile.yaml")

wrapIOError :: FilePath -> IO a -> IO a
wrapIOError cfgDir = handle @IOError (throwIO . ProfileIOError cfgDir)

data ProfileMode = BuildMode | RunMode
  deriving (Show, Enum, Bounded)

data Directories = MkDirectories {cfgDir, dataDir, cacheDir, logDir :: !FilePath}
  deriving (Show)

_profileService :: ProfileCfg -> Directories -> ProfileMode -> Service
_profileService ProfileCfg{..} MkDirectories{..} = \case
  BuildMode ->
    MkService
      { serviceType = Simple
      , description = T.pack "Build script for " <> profileProps.profileDetails
      , stdOutput = JournalWConsole
      , stdErr = JournalWConsole
      , execStart = cfgDir </> buildScript
      , wantedBy = []
      }
  RunMode ->
    MkService
      { serviceType = Simple
      , description = profileProps.profileDetails
      , stdOutput = Journal
      , stdErr = Journal
      , execStart = cfgDir </> runScript
      , wantedBy = []
      }

-- Load profile with the ID.
loadProfile :: ManageEnv -> FilePath -> IO (Component ProfileMode, ID)
loadProfile ManageEnv{..} cfgDir = profileOf <$> readProfileCfg cfgDir
  where
    locFor ident str = envPath </> str </> idStr ident
    cfgFor path = cfgDir </> path
    profileOf cfg@ProfileCfg{..} = (profile, profileID)
      where
        dirs =
          MkDirectories
            { cfgDir = cfgDir
            , dataDir = locFor profileID "data"
            , cacheDir = locFor profileID "cache"
            , logDir = locFor profileID "logs"
            }

        profile = deps <> prepDirectory <> prepSession <> setupEnv <> scripts <> buildOnInstall
        deps = ofDependencies dependencies
        scripts = fromScript executeScript (cfgFor <$> installScript) Nothing $ \case
          BuildMode -> cfgFor buildScript
          RunMode -> cfgFor runScript

        prepDirectory = ofHandle $ prepareDirectory dirs
        prepSession = ofHandle $ prepareSession cfg dirs
        setupEnv = ofHandle $ setupEnvironment dirs
        -- Installation should finish with building the profile.
        buildOnInstall = ofHandle $ \mEnv -> \case
          CustomInstall -> invoke mEnv BuildMode profile
          _ -> pure ()

-- | Executes the respective script, case-by-case basis since given args could be changed.
executeScript :: ManageEnv -> FilePath -> Context ProfileMode -> IO ()
executeScript ManageEnv{..} script = \case
  CustomInstall -> do
    logger "Install using %s..." script
    callProcess script []
  CustomRemove -> do
    logger "Remove using %s..." script
    callProcess script []
  InvokeOn BuildMode -> do
    logger "Build using %s..." script
    callProcess script []
  -- callProcess "systemctl" ["start", script]
  InvokeOn RunMode -> do
    logger "Run using %s..." script
    callProcess script []

setupEnvironment :: Directories -> ManageEnv -> Context a -> IO ()
setupEnvironment MkDirectories{..} ManageEnv{..} = \case
  CustomInstall -> pure ()
  CustomRemove -> pure ()
  InvokeOn _ -> do
    setEnv "ENV_ARCH" arch >> setEnv "ENV_OS" os
    setEnv "XMONAD_DATA_DIR" dataDir
    setEnv "XMONAD_CONFIG_DIR" cfgDir
    setEnv "XMONAD_CACHE_DIR" cacheDir
    setEnv "XMONAD_LOG_DIR" logDir

prepareDirectory :: Directories -> ManageEnv -> Context a -> IO ()
prepareDirectory MkDirectories{..} ManageEnv{..} = \case
  CustomInstall -> do
    logger "Preparing profile directories..."
    traverse_ (createDirectoryIfMissing True) [dataDir, cacheDir, logDir]
  CustomRemove -> do
    logger "Removing profile directories..."
    traverse_ removePathForcibly [dataDir, cacheDir, logDir]
  InvokeOn _ -> pure ()

prepareSession :: ProfileCfg -> Directories -> ManageEnv -> Context a -> IO ()
prepareSession ProfileCfg{..} MkDirectories{..} ManageEnv{..} = \case
  CustomInstall -> do
    logger "Installing xsession runner..."
    writeFile deskEntryPath deskEntry
    -- Instead of linking, we copy the runner. Fixes issues with SDDM.
    callProcess "sudo" ["cp", "-T", deskEntryPath, sessionPath]
  CustomRemove -> do
    logger "Removing xsession runner..."
    callProcess "sudo" ["rm", "-f", sessionPath]
  InvokeOn _ -> pure ()
  where
    sessionPath = "/" </> "usr" </> "share" </> "xsessions" </> idStr profileID <.> "desktop"
    deskEntryPath = dataDir </> "run" <.> "desktop"
    deskEntry =
      unlines
        [ printf "[Desktop Entry]"
        , printf "Encoding=UTF-8"
        , printf "Type=XSession"
        , printf "Name=%s" profileProps.profileName
        , printf "Comment=%s" profileProps.profileDetails
        , printf "Exec=%s" runCmd
        ]

    runCmd :: String =
      printf
        "env PATH=%s xmonad-manage run %s > %s 2> %s"
        "\\$HOME/.cabal/bin:\\$HOME/.ghcup/bin:\\$PATH"
        (idStr profileID)
        (show $ logDir </> "start.log")
        (show $ logDir </> "start.err")
