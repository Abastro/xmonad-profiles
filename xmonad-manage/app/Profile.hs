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

-- serviceDir is not specific to the config, but this does make things convenient.
data Directories = MkDirectories {cfgDir, dataDir, cacheDir, logDir, serviceDir :: !FilePath}
  deriving (Show)

-- Load profile with the ID.
loadProfile :: ManageEnv -> FilePath -> IO (Component ProfileMode, ID)
loadProfile mEnv@ManageEnv{..} cfgDir = do
  spec@ProfileSpec{profileID} <- readProfileSpec cfgDir
  serviceDir <- getServiceDirectory PerUserService
  pure $ profileForSpec mEnv (dirsOf profileID serviceDir) spec
  where
    locFor ident str = envPath </> str </> idStr ident
    dirsOf profileID serviceDir =
      MkDirectories
        { cfgDir = cfgDir
        , dataDir = locFor profileID "data"
        , cacheDir = locFor profileID "cache"
        , logDir = locFor profileID "logs"
        , serviceDir
        }

profileForSpec :: ManageEnv -> Directories -> ProfileSpec -> (Component ProfileMode, ID)
profileForSpec ManageEnv{..} dirs@MkDirectories{..} cfg@ProfileSpec{..} = (profile, profileID)
  where
    profile = deps <> prepDirectory <> prepSession <> setupEnv <> useService <> scripts <> buildOnInstall
    deps = ofDependencies dependencies
    scripts =
      fromScript
        executeScript
        ( \case
            Install -> cfgFor <$> installScript
            Remove -> Nothing
        )
        ( \case
            BuildMode -> cfgFor buildScript
            RunMode -> cfgFor runService
        )
    cfgFor path = cfgDir </> path

    setupEnv = ofHandle $ setupEnvironment dirs
    useService = ofHandle $ handleService cfg dirs
    prepDirectory = ofHandle $ prepareDirectory dirs
    prepSession = ofHandle $ prepareSession cfg dirs
    -- Installation should finish with building the profile.
    buildOnInstall = ofHandle $ \mEnv -> \case
      Custom Install -> invoke mEnv BuildMode profile
      _ -> pure ()

-- | Executes the respective script, case-by-case basis since given args could be changed.
executeScript :: ManageEnv -> FilePath -> Context ProfileMode -> IO ()
executeScript ManageEnv{..} script = \case
  Custom phase -> do
    case phase of
      Install -> logger "Install using %s..." script
      Remove -> logger "Remove using %s..." script
    callProcess script []
  InvokeOn BuildMode -> do
    -- It would be great to log to journal, but eh.. not needed anyway.
    logger "Build through script %s..." script
    callProcess script []
  InvokeOn RunMode -> pure () -- Services call the scripts, instead.

environments :: Directories -> M.Map String String
environments MkDirectories{..} =
  M.fromList
    [ ("XMONAD_NAME", "xmonad-" <> arch <> "-" <> os)
    , ("XMONAD_DATA_DIR", dataDir)
    , ("XMONAD_CONFIG_DIR", cfgDir)
    , ("XMONAD_CACHE_DIR", cacheDir)
    , ("XMONAD_LOG_DIR", logDir)
    ]

setupEnvironment :: Directories -> ManageEnv -> Context ProfileMode -> IO ()
setupEnvironment dirs@MkDirectories{..} ManageEnv{..} mode = for_ (M.toList $ environments dirs) (uncurry putEnv)
  where
    putEnv = case mode of
      InvokeOn RunMode -> setServiceEnv
      _ -> setEnv

prepareDirectory :: Directories -> ManageEnv -> Context a -> IO ()
prepareDirectory MkDirectories{..} ManageEnv{..} = \case
  Custom Install -> do
    logger "Preparing profile directories..."
    traverse_ (createDirectoryIfMissing True) [dataDir, cacheDir]
  Custom Remove -> do
    logger "Removing profile directories..."
    traverse_ removePathForcibly [dataDir, cacheDir]
  InvokeOn _ -> pure ()

handleService :: ProfileSpec -> Directories -> ManageEnv -> Context ProfileMode -> IO ()
handleService ProfileSpec{..} dirs@MkDirectories{..} ManageEnv{..} = \case
  Custom Install -> do
    createDirectoryIfMissing True serviceDir
  --
  Custom Remove -> do
    logger "Removing systemd services..."
    traverse_ removeService (runService : otherServices)
  --
  InvokeOn BuildMode -> do
    -- Install on build to allow frequent changes of services
    logger "Installing systemd services..."
    traverse_ setupService (runService : otherServices)
    callProcess "systemctl" ["--user", "daemon-reload"]
    logger "Systemd user daemon reloaded."
  --
  InvokeOn RunMode -> do
    logger "Run through service %s..." (serviceNameOf runService)
    callProcess "systemctl" ["--user", "start", "--wait", serviceNameOf runService]
  where
    -- Setup service based on "template" at the path.
    setupService templatePath = do
      let serviceName = serviceNameOf templatePath
      logger "Service %s being installed." serviceName
      template <- T.readFile (cfgDir </> templatePath) >>= parseShellString serviceName
      service <- shellExpandWith (readEnv . T.unpack) template
      T.writeFile (serviceDir </> serviceName) service

    removeService templatePath = do
      let serviceName = serviceNameOf templatePath
      logger "Service %s being removed." serviceName
      removeFile (serviceDir </> serviceName)

    serviceNameOf templatePath = snd (splitFileName templatePath)

    serviceEnvs = M.insert "HOME" home $ environments dirs
    readEnv envName = case serviceEnvs M.!? envName of
      Just val -> pure (T.pack val)
      Nothing -> fail $ printf "Environment variable %s not found" envName

prepareSession :: ProfileSpec -> Directories -> ManageEnv -> Context a -> IO ()
prepareSession ProfileSpec{..} MkDirectories{..} ManageEnv{..} = \case
  Custom Install -> do
    logger "Installing xsession desktop entry..."
    template <- T.readFile desktopTemplatePath >>= parseShellString "desktop entry template"
    desktopEntry <- shellExpandWith (readEnv . T.unpack) template
    T.writeFile intermediatePath desktopEntry
    -- Instead of linking, we copy the runner. Fixes issues with SDDM.
    callProcess "sudo" ["cp", "-T", intermediatePath, sessionPath]
  Custom Remove -> do
    logger "Removing xsession runner..."
    callProcess "sudo" ["rm", "-f", sessionPath]
  InvokeOn _ -> pure () -- Session is not something to invoke
  where
    desktopTemplatePath = envPath </> "database" </> "template" <.> "desktop"
    sessionPath = "/usr" </> "share" </> "xsessions" </> idStr profileID <.> "desktop"
    intermediatePath = dataDir </> "run" <.> "desktop"

    profileEnv =
      M.fromList
        [ ("PROFILE_ID", T.pack $ idStr profileID)
        , ("PROFILE_NAME", profileProps.profileName)
        , ("PROFILE_DETAILS", profileProps.profileDetails)
        ]
    readEnv envName = case profileEnv M.!? envName of
      Just val -> pure val
      Nothing -> fail $ printf "Profile variable %s not found" envName
