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
      <$> (m .: "ID")
      <*> (ProfileProps <$> m .: "name" <*> m .: "details")
      <*> (fmap T.unpack <$> m .:? "install")
      <*> (T.unpack <$> m .: "build")
      <*> (T.unpack <$> m .: "run-service")
      <*> (map T.unpack <$> m .:? "other-services" .!= [])
      <*> (m .:? "dependencies" .!= [])

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
loadProfile :: FilePath -> IO (Component ProfileMode, ID)
loadProfile configDir = do
  spec <- readProfileSpec configDir
  pure (profileForSpec configDir spec, spec.profileID)

profileForSpec :: FilePath -> ProfileSpec -> Component ProfileMode
profileForSpec configDir spec = profile
  where
    profile =
      mconcat
        [ ofDependencies spec.dependencies
        , ofAction (getDirectories configDir spec)
            >>> mconcat
              [ ofHandle prepareProfileDirs
              , ofHandle (prepareSession spec)
              , ofHandle setupEnvironment
              , serviceModule spec
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
      Custom Install -> invoke mEnv BuildMode profile -- (Tying the knot)
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
environments dirs =
  M.fromList
    [ ("XMONAD_NAME", T.pack $ "xmonad-" <> arch <> "-" <> os)
    , ("XMONAD_DATA_DIR", T.pack dirs.dataDir)
    , ("XMONAD_CONFIG_DIR", T.pack dirs.configDir)
    , ("XMONAD_CACHE_DIR", T.pack dirs.cacheDir)
    , ("XMONAD_LOG_DIR", T.pack dirs.logDir)
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
prepareProfileDirs dirs = \case
  Custom Install -> do
    printf "Preparing profile directories...\n"
    traverse_ (createDirectoryIfMissing True) [dirs.dataDir, dirs.cacheDir]
  Custom Remove -> do
    printf "Removing profile directories...\n"
    traverse_ removePathForcibly [dirs.dataDir, dirs.cacheDir]
  InvokeOn _ -> pure ()

setupEnvironment :: Directories -> Context ProfileMode -> IO ()
setupEnvironment dirs mode = traverse_ (uncurry putEnv) (M.toList $ environments dirs)
  where
    putEnv = case mode of
      InvokeOn RunMode -> setServiceEnv
      _ -> setNormalEnv

prepareSession :: ProfileSpec -> Directories -> Context a -> IO ()
prepareSession ProfileSpec{..} dirs = \case
  Custom Install -> do
    printf "Installing xsession desktop entry...\n"
    -- TODO This could be simplified
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
    desktopTemplatePath = dirs.databaseDir </> "template" <.> "desktop"
    sessionPath = "/usr" </> "share" </> "xsessions" </> idStr profileID <.> "desktop"
    intermediatePath = dirs.dataDir </> "run" <.> "desktop"

    onEnvNotFound key = fail $ printf "Profile variable %s not found" key
    profileEnv =
      M.fromList
        [ ("PROFILE_ID", T.pack $ idStr profileID)
        , ("PROFILE_NAME", profileProps.profileName)
        , ("PROFILE_DETAILS", profileProps.profileDetails)
        ]

serviceNameOf templatePath = snd (splitFileName templatePath)

serviceModule :: ProfileSpec -> ComponentCat ProfileMode Directories ()
serviceModule spec = ofHandle declare <> installAllServices <> ofHandle apply
  where
    installAllServices = traverse_ (ofHandle . installService) (spec.runService : spec.otherServices)

    declare _ = \case
      Custom Install -> pure ()
      Custom Remove -> printf "Removing systemd services...\n"
      InvokeOn BuildMode -> printf "Installing systemd services...\n"
      InvokeOn RunMode -> printf "Run through service %s...\n" (serviceNameOf spec.runService)

    apply dirs = \case
      Custom Install -> createDirectoryIfMissing True dirs.serviceDir
      Custom Remove -> pure ()
      InvokeOn BuildMode -> do
        callProcess "systemctl" ["--user", "daemon-reload"]
        printf "Systemd user daemon reloaded.\n"
      InvokeOn RunMode -> do
        callProcess "systemctl" ["--user", "start", "--wait", serviceNameOf spec.runService]

installService :: FilePath -> Directories -> Context ProfileMode -> IO ()
installService templatePath dirs = \case
  -- Install on build to allow frequent changes of services
  InvokeOn BuildMode -> do
    printf "Service %s being installed.\n" serviceName
    template <- T.readFile (dirs.configDir </> templatePath) >>= parseShellString serviceName
    service <- shellExpandFromMap onEnvNotFound (environments dirs) template
    T.writeFile (dirs.serviceDir </> serviceName) service
  Custom Remove -> do
    printf "Service %s being removed.\n" serviceName
    removeFile (dirs.serviceDir </> serviceName)
  _ -> pure ()
  where
    serviceName = serviceNameOf templatePath
    onEnvNotFound key = fail $ printf "Environment variable %s not found." key
