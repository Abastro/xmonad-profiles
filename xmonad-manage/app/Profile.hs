module Profile
  ( InstalledProfiles,
    addProfile,
    removeProfile,
    getProfilePath,
    profilePaths,
    ProfileSpec (..),
    ProfileError (..),
    ProfileMode (..),
    readProfileSpec,
    loadProfile,
  )
where

import Common
import Component
import Control.Exception
import Data.Foldable
import Data.Map.Strict qualified as M
import Data.Proxy
import Data.Serialize
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.YAML
import GHC.Generics
import Manages
import Packages
import References
import System.Directory
import System.FilePath
import System.Info (arch, os)
import System.Process
import Text.Printf

newtype InstalledProfiles = InstalledProfiles
  { profiles :: M.Map ID FilePath
  }
  deriving (Show, Generic)

instance Serialize InstalledProfiles

instance SavedData InstalledProfiles where
  dataName :: Proxy InstalledProfiles -> FilePath
  dataName Proxy = "installed-profiles"
  initialize :: IO InstalledProfiles
  initialize = pure $ InstalledProfiles M.empty

addProfile :: ID -> FilePath -> InstalledProfiles -> InstalledProfiles
addProfile ident path ips = ips {profiles = M.insert ident path ips.profiles}

removeProfile :: ID -> InstalledProfiles -> InstalledProfiles
removeProfile ident ips = ips {profiles = M.delete ident ips.profiles}

getProfilePath :: ID -> InstalledProfiles -> Maybe FilePath
getProfilePath ident ips = ips.profiles M.!? ident

profilePaths :: InstalledProfiles -> [FilePath]
profilePaths ips = M.elems ips.profiles

data ProfileSpec = ProfileSpec
  { profileID :: !ID,
    profileName :: !T.Text,
    profileDetails :: !T.Text,
    installScript :: !(Maybe FilePath),
    buildScript, runService :: !FilePath,
    otherServices :: [FilePath],
    dependencies :: [Package]
  }
  deriving (Show)

instance FromYAML ProfileSpec where
  parseYAML :: Node Pos -> Parser ProfileSpec
  parseYAML = withMap "profile" $ \m ->
    ProfileSpec
      <$> (m .: "ID")
      <*> (m .: "name")
      <*> (m .: "details")
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
readProfileSpec cfgDir = wrapIOError $ readYAMLFile ProfileWrongFormat (cfgDir </> "profile.yaml")
  where
    wrapIOError = handle @IOError (throwIO . ProfileIOError cfgDir)

data Directories = MkDirectories
  { configDir, dataDir, cacheDir, serviceDir, databaseDir :: !FilePath
  }
  deriving (Show)

-- | Load profile with its ID.
loadProfile :: FilePath -> IO (Component ProfileMode)
loadProfile configDir = profileForSpec configDir <$> readProfileSpec configDir

profileForSpec :: FilePath -> ProfileSpec -> Component ProfileMode
profileForSpec configDir spec = profile
  where
    profile =
      withIdentifier spec.profileID $
        mconcat
          [ ofDependencies spec.dependencies,
            ofAction (getDirectories configDir spec)
              >>> mconcat
                [ ofHandle prepareProfileDirs,
                  ofHandle (prepareSession spec),
                  ofHandle setupEnvironment,
                  serviceModule spec
                ],
            scripts >>> traversing_ (ofHandle $ executeScript spec.profileName),
            buildOnInstall
          ]
    scripts = executableScripts $ \case
      Custom Install -> cfgFor <$> spec.installScript
      Custom Remove -> Nothing
      InvokeOn BuildMode -> Just (cfgFor spec.buildScript)
      InvokeOn RunMode -> Nothing

    cfgFor path = configDir </> path
    -- Installation should finish with building the profile.
    buildOnInstall = ofHandle $ \mEnv -> \case
      Custom Install -> invoke mEnv BuildMode profile -- (Tying the knot)
      _ -> pure ()

-- | Executes the respective script, case-by-case basis since given args could be changed.
executeScript :: T.Text -> FilePath -> Context ProfileMode -> IO ()
executeScript name script = \case
  Custom Install -> do
    logOn $ Custom Install
    callProcess script []
  Custom Remove -> do
    logOn $ Custom Remove
    callProcess script []
  InvokeOn BuildMode -> do
    -- It would be great to log to journal, but eh.. not needed anyway.
    logOn $ InvokeOn BuildMode
    callProcess script []
  InvokeOn RunMode -> pure () -- Services call the scripts, instead.
  where
    logOn mode = profileLog name (ProfileScriptPhase mode script)

environments :: Directories -> M.Map String T.Text
environments dirs =
  M.fromList
    [ ("XMONAD_NAME", T.pack $ "xmonad-" <> arch <> "-" <> os),
      ("XMONAD_DATA_DIR", T.pack dirs.dataDir),
      ("XMONAD_CONFIG_DIR", T.pack dirs.configDir),
      ("XMONAD_CACHE_DIR", T.pack dirs.cacheDir)
    ]

getDirectories :: FilePath -> ProfileSpec -> ManageEnv -> IO Directories
getDirectories configDir spec mEnv = do
  dataDir <- getXdgDirectory XdgData (idStr spec.profileID)
  -- Apparently the executable should be in a cache..
  cacheDir <- getXdgDirectory XdgCache (idStr spec.profileID)
  serviceDir <- getServiceDirectory PerUserService
  pure MkDirectories {databaseDir = mEnv.databaseDir, ..}

prepareProfileDirs :: Directories -> Context a -> IO ()
prepareProfileDirs dirs = \case
  Custom Install -> do
    profileLog undefined $ PrepareDirectories Install
    traverse_ (createDirectoryIfMissing True) [dirs.dataDir, dirs.cacheDir]
  Custom Remove -> do
    profileLog undefined $ PrepareDirectories Remove
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
    profileLog profileName $ PrepareSession Install
    template <- readShellStringFile desktopTemplatePath
    desktopEntry <- shellExpandFromMap onEnvNotFound profileEnv template
    T.writeFile intermediatePath desktopEntry
    -- Instead of moving it around, we copy the runner. Fixes issues with SDDM.
    callProcess "sudo" ["cp", "-T", intermediatePath, sessionPath]
  Custom Remove -> do
    profileLog profileName $ PrepareSession Remove
    callProcess "sudo" ["rm", "-f", sessionPath]
  InvokeOn _ -> pure () -- Session is not something to invoke
  where
    desktopTemplatePath = dirs.databaseDir </> "template" <.> "desktop"
    sessionPath = "/usr" </> "share" </> "xsessions" </> idStr profileID <.> "desktop"
    intermediatePath = dirs.dataDir </> "run" <.> "desktop"

    onEnvNotFound key = fail $ printf "Profile variable %s not found" key
    profileEnv =
      M.fromList
        [ ("PROFILE_ID", T.pack $ idStr profileID),
          ("PROFILE_NAME", profileName),
          ("PROFILE_DETAILS", profileDetails)
        ]

serviceNameOf :: FilePath -> String
serviceNameOf templatePath = snd (splitFileName templatePath)

serviceModule :: ProfileSpec -> ComponentCat ProfileMode Directories ()
serviceModule spec = ofHandle declare <> installAllServices <> ofHandle apply
  where
    installAllServices = traverse_ (ofHandle . installService profileName) (spec.runService : spec.otherServices)
    profileName = spec.profileName

    declare _ = \case
      Custom Install -> pure ()
      Custom Remove -> profileLog profileName $ PrepareServices Remove
      InvokeOn BuildMode -> profileLog profileName $ PrepareServices Install
      InvokeOn RunMode -> profileLog profileName $ RunMainService (serviceNameOf spec.runService)

    apply dirs = \case
      Custom Install -> createDirectoryIfMissing True dirs.serviceDir
      Custom Remove -> pure ()
      InvokeOn BuildMode -> do
        callProcess "systemctl" ["--user", "daemon-reload"]
        printf "Systemd user daemon reloaded.\n"
      InvokeOn RunMode -> do
        callProcess "systemctl" ["--user", "start", "--wait", serviceNameOf spec.runService]

installService :: T.Text -> FilePath -> Directories -> Context ProfileMode -> IO ()
installService profileName templatePath dirs = \case
  -- Install on build to allow frequent changes of services
  InvokeOn BuildMode -> do
    profileLog profileName $ InstallService Install serviceName
    template <- readShellStringFile (dirs.configDir </> templatePath)
    service <- shellExpandFromMap onEnvNotFound (environments dirs) template
    T.writeFile (dirs.serviceDir </> serviceName) service
  Custom Remove -> do
    profileLog profileName $ InstallService Remove serviceName
    removeFile (dirs.serviceDir </> serviceName)
  _ -> pure ()
  where
    serviceName = serviceNameOf templatePath
    onEnvNotFound key = fail $ printf "Service variable %s not found." key
