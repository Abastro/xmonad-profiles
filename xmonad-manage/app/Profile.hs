module Profile (
  ProfileProps (..),
  ProfileSpec (..),
  ProfileError (..),
  ProfileMode (..),
  readProfileSpec,
  loadProfile,
) where

import Common
import Component
import Control.Exception
import Data.Foldable
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.YAML
import Manages
import Packages
import System.Directory
import System.FilePath
import System.Info (arch, os)
import System.Process
import Text.Printf

data ProfileProps = ProfileProps
  { profileName :: !T.Text
  , profileDetails :: !T.Text
  }
  deriving (Show)

data ProfileSpec = ProfileSpec
  { profileID :: !ID
  , profileProps :: !ProfileProps
  , installScript :: !(Maybe FilePath)
  , buildScript, runService :: !FilePath
  , otherServices :: [FilePath]
  , dependencies :: [Package]
  }
  deriving (Show)

instance FromYAML ProfileSpec where
  parseYAML :: Node Pos -> Parser ProfileSpec
  parseYAML = withMap "profile" $ \m ->
    ProfileSpec
      <$> (m .: T.pack "ID")
      <*> (ProfileProps <$> m .: T.pack "name" <*> m .: T.pack "details")
      <*> (fmap T.unpack <$> m .:? T.pack "install")
      <*> (T.unpack <$> m .: T.pack "build")
      <*> (T.unpack <$> m .: T.pack "run-service")
      <*> (map T.unpack <$> m .:? T.pack "other-services" .!= [])
      <*> (m .:? T.pack "dependencies" .!= [])

data ProfileError
  = ProfileNotFound ID
  | -- | IO Error while "loading" the profile.
    ProfileIOError FilePath IOError
  | ProfileWrongFormat String
  deriving (Show)

instance Exception ProfileError

readProfileSpec :: FilePath -> IO ProfileSpec
readProfileSpec cfgDir = wrapIOError cfgDir $ readYAMLFile ProfileWrongFormat (cfgDir </> "profile.yaml")

wrapIOError :: FilePath -> IO a -> IO a
wrapIOError cfgDir = handle @IOError (throwIO . ProfileIOError cfgDir)

data ProfileMode = BuildMode | RunMode
  deriving (Show, Eq, Enum, Bounded)

data Directories = MkDirectories
  { configDir, dataDir, cacheDir, logDir, serviceDir, databaseDir :: !FilePath
  }
  deriving (Show)

-- | Load profile with its ID.
loadProfile :: ManageEnv -> FilePath -> IO (Component ProfileMode, ID)
loadProfile mEnv@ManageEnv{..} configDir = do
  spec <- readProfileSpec configDir
  pure (profileForSpec mEnv configDir spec, spec.profileID)

profileForSpec :: ManageEnv -> FilePath -> ProfileSpec -> Component ProfileMode
profileForSpec ManageEnv{..} configDir spec = profile
  where
    profile =
      mconcat
        [ ofDependencies spec.dependencies
        , ofAction (getDirectories configDir spec)
            >>> mconcat
              [ ofHandle prepareProfileDirs
              , ofHandle (prepareSession spec)
              , ofHandle setupEnvironment
              , ofHandle (handleService spec)
              ]
        , scripts
        , buildOnInstall
        ]
    scripts =
      fromScript
        executeScript
        ( \case
            Install -> cfgFor <$> spec.installScript
            Remove -> Nothing
        )
        ( \case
            BuildMode -> cfgFor spec.buildScript
            RunMode -> cfgFor spec.runService
        )
    cfgFor path = configDir </> path
    -- Installation should finish with building the profile.
    buildOnInstall = ofHandle $ \mEnv -> \case
      Custom Install -> invoke mEnv BuildMode profile
      _ -> pure ()

-- | Executes the respective script, case-by-case basis since given args could be changed.
executeScript :: ManageEnv -> FilePath -> Context ProfileMode -> IO ()
executeScript _ script = \case
  Custom phase -> do
    case phase of
      Install -> printf "Install using %s...\n" script
      Remove -> printf "Remove using %s...\n" script
    callProcess script []
  InvokeOn BuildMode -> do
    -- It would be great to log to journal, but eh.. not needed anyway.
    printf "Build through script %s...\n" script
    callProcess script []
  InvokeOn RunMode -> pure () -- Services call the scripts, instead.

environments :: Directories -> M.Map String T.Text
environments MkDirectories{..} =
  M.fromList
    [ ("XMONAD_NAME", T.pack $ "xmonad-" <> arch <> "-" <> os)
    , ("XMONAD_DATA_DIR", T.pack dataDir)
    , ("XMONAD_CONFIG_DIR", T.pack configDir)
    , ("XMONAD_CACHE_DIR", T.pack cacheDir)
    , ("XMONAD_LOG_DIR", T.pack logDir)
    ]

getDirectories :: FilePath -> ProfileSpec -> ManageEnv -> IO Directories
getDirectories configDir spec mEnv = do
  serviceDir <- getServiceDirectory PerUserService
  let locFor str = mEnv.envPath </> str </> idStr spec.profileID
  pure
    MkDirectories
      { configDir
      , dataDir = locFor "data"
      , cacheDir = locFor "cache"
      , logDir = locFor "logs"
      , serviceDir
      , databaseDir = mEnv.envPath </> "database"
      }

prepareProfileDirs :: Directories -> Context a -> IO ()
prepareProfileDirs MkDirectories{..} = \case
  Custom Install -> do
    printf "Preparing profile directories...\n"
    traverse_ (createDirectoryIfMissing True) [dataDir, cacheDir]
  Custom Remove -> do
    printf "Removing profile directories...\n"
    traverse_ removePathForcibly [dataDir, cacheDir]
  InvokeOn _ -> pure ()

setupEnvironment :: Directories -> Context ProfileMode -> IO ()
setupEnvironment dirs mode = for_ (M.toList $ environments dirs) (uncurry putEnv)
  where
    putEnv = case mode of
      InvokeOn RunMode -> setServiceEnv
      _ -> setNormalEnv

prepareSession :: ProfileSpec -> Directories -> Context a -> IO ()
prepareSession ProfileSpec{..} MkDirectories{..} = \case
  Custom Install -> do
    printf "Installing xsession desktop entry...\n"
    template <- T.readFile desktopTemplatePath >>= parseShellString "desktop entry template"
    desktopEntry <- shellExpandFromMap onEnvNotFound profileEnv template
    T.writeFile intermediatePath desktopEntry
    -- Instead of linking, we copy the runner. Fixes issues with SDDM.
    callProcess "sudo" ["cp", "-T", intermediatePath, sessionPath]
  Custom Remove -> do
    printf "Removing xsession runner...\n"
    callProcess "sudo" ["rm", "-f", sessionPath]
  InvokeOn _ -> pure () -- Session is not something to invoke
  where
    desktopTemplatePath = databaseDir </> "template" <.> "desktop"
    sessionPath = "/usr" </> "share" </> "xsessions" </> idStr profileID <.> "desktop"
    intermediatePath = dataDir </> "run" <.> "desktop"

    onEnvNotFound key = fail $ printf "Profile variable %s not found" key
    profileEnv =
      M.fromList
        [ ("PROFILE_ID", T.pack $ idStr profileID)
        , ("PROFILE_NAME", profileProps.profileName)
        , ("PROFILE_DETAILS", profileProps.profileDetails)
        ]

handleService :: ProfileSpec -> Directories -> Context ProfileMode -> IO ()
handleService ProfileSpec{..} dirs@MkDirectories{..} = \case
  Custom Install -> do
    createDirectoryIfMissing True serviceDir
  --
  Custom Remove -> do
    printf "Removing systemd services...\n"
    traverse_ removeService (runService : otherServices)
  --
  InvokeOn BuildMode -> do
    -- Install on build to allow frequent changes of services
    printf "Installing systemd services...\n"
    traverse_ setupService (runService : otherServices)
    callProcess "systemctl" ["--user", "daemon-reload"]
    printf "Systemd user daemon reloaded.\n"
  --
  InvokeOn RunMode -> do
    printf "Run through service %s..." (serviceNameOf runService)
    callProcess "systemctl" ["--user", "start", "--wait", serviceNameOf runService]
  where
    -- Setup service based on "template" at the path.
    setupService templatePath = do
      let serviceName = serviceNameOf templatePath
      printf "Service %s being installed.\n" serviceName
      template <- T.readFile (configDir </> templatePath) >>= parseShellString serviceName
      service <- shellExpandFromMap onEnvNotFound (environments dirs) template
      T.writeFile (serviceDir </> serviceName) service

    removeService templatePath = do
      let serviceName = serviceNameOf templatePath
      printf "Service %s being removed.\n" serviceName
      removeFile (serviceDir </> serviceName)

    serviceNameOf templatePath = snd (splitFileName templatePath)
    onEnvNotFound key = fail $ printf "Environment variable %s not found." key
