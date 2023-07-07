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
  , buildScript, runService :: !FilePath
  , otherServices :: [FilePath]
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

readProfileCfg :: FilePath -> IO ProfileCfg
readProfileCfg cfgDir = wrapIOError cfgDir $ readYAMLFile ProfileWrongFormat (cfgDir </> "profile.yaml")

wrapIOError :: FilePath -> IO a -> IO a
wrapIOError cfgDir = handle @IOError (throwIO . ProfileIOError cfgDir)

data ProfileMode = BuildMode | RunMode
  deriving (Show, Eq, Enum, Bounded)

data Directories = MkDirectories {cfgDir, dataDir, cacheDir, logDir :: !FilePath}
  deriving (Show)

profileDeskEntry :: ProfileCfg -> FilePath -> String
profileDeskEntry ProfileCfg{..} startPath =
  unlines
    [ printf "[Desktop Entry]"
    , printf "Encoding=UTF-8"
    , printf "Type=XSession"
    , printf "Name=%s" profileProps.profileName
    , printf "Comment=%s" profileProps.profileDetails
    , printf "Exec=%s" startPath
    ]

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

        profile = deps <> prepDirectory <> prepSession <> setupEnv <> useService <> scripts <> buildOnInstall
        deps = ofDependencies dependencies
        scripts = fromScript executeScript (cfgFor <$> installScript) Nothing $ \case
          BuildMode -> cfgFor buildScript
          RunMode -> cfgFor runService

        setupEnv = ofHandle $ setupEnvironment dirs
        useService = ofHandle $ handleService cfg dirs
        prepDirectory = ofHandle $ prepareDirectory dirs
        prepSession = ofHandle $ prepareSession cfg dirs
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
    -- It would be great to log to journal, but eh.. not needed anyway.
    logger "Build using %s..." script
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
setupEnvironment dirs@MkDirectories{..} ManageEnv{..} = \case
  CustomInstall -> pure ()
  CustomRemove -> pure ()
  InvokeOn mode -> for_ (M.toList $ environments dirs) (uncurry $ putEnv mode)
  where
    putEnv = \case
      BuildMode -> setEnv
      RunMode -> setServiceEnv

prepareDirectory :: Directories -> ManageEnv -> Context a -> IO ()
prepareDirectory MkDirectories{..} ManageEnv{..} = \case
  CustomInstall -> do
    logger "Preparing profile directories..."
    traverse_ (createDirectoryIfMissing True) [dataDir, cacheDir, logDir]
  CustomRemove -> do
    logger "Removing profile directories..."
    traverse_ removePathForcibly [dataDir, cacheDir, logDir]
  InvokeOn _ -> pure ()

handleService :: ProfileCfg -> Directories -> ManageEnv -> Context ProfileMode -> IO ()
handleService ProfileCfg{..} dirs@MkDirectories{..} ManageEnv{..} = \case
  CustomInstall -> do
    logger "Installing systemd services..."
    createDirectoryIfMissing True serviceDir
  CustomRemove -> do
    logger "Removing systemd services..."
    traverse_ removeService (runService : otherServices)
  InvokeOn BuildMode -> do
    -- Install on build to allow frequent changes of services
    logger "Installing systemd services..."
    traverse_ setupService (runService : otherServices)
    callProcess "systemctl" ["--user", "daemon-reload"]
    logger "Systemd user daemon reloaded."
  InvokeOn RunMode -> do
    logger "Run through service %s..." (serviceNameOf runService)
    callProcess "systemctl" ["--user", "start", "--wait", serviceNameOf runService]
  where
    -- Setup service based on "template" at the path.
    setupService templatePath = do
      let serviceName = serviceNameOf templatePath
      template <- T.readFile (cfgDir </> templatePath) >>= parseShellString serviceName
      service <- shellExpandWith (readEnv home . T.unpack) template
      T.writeFile (serviceDir </> serviceName) service
    removeService templatePath = removeFile (serviceDir </> serviceNameOf templatePath)

    serviceNameOf templatePath = snd (splitFileName templatePath)
    serviceDir = home </> ".config" </> "systemd" </> "user"

    serviceEnvs home dirs = M.insert "HOME" home $ environments dirs
    readEnv home envName = case serviceEnvs home dirs M.!? envName of
      Just val -> pure (T.pack val)
      Nothing -> fail $ "Environment variable" <> envName <> "not found"

prepareSession :: ProfileCfg -> Directories -> ManageEnv -> Context a -> IO ()
prepareSession cfg@ProfileCfg{..} MkDirectories{..} ManageEnv{..} = \case
  CustomInstall -> do
    logger "Installing xsession runner..."
    writeFile startPath startScript
    setToExecutable startPath
    writeFile deskEntryPath (profileDeskEntry cfg startPath)
    -- Instead of linking, we copy the runner. Fixes issues with SDDM.
    callProcess "sudo" ["cp", "-T", deskEntryPath, sessionPath]
  CustomRemove -> do
    logger "Removing xsession runner..."
    callProcess "sudo" ["rm", "-f", sessionPath]
  InvokeOn _ -> pure () -- Session is not something to invoke
  where
    sessionPath = "/" </> "usr" </> "share" </> "xsessions" </> idStr profileID <.> "desktop"
    deskEntryPath = dataDir </> "run" <.> "desktop"

    startPath = dataDir </> "starter.sh"
    startScript =
      unlines
        [ printf "#!/bin/sh"
        , printf "export PATH=$HOME/.cabal/bin:$HOME/.ghcup/bin:$PATH"
        , printf "systemctl --user import-environment PATH" -- Blame ghcup for not putting environment inside .profile
        , printf
            "exec xmonad-manage run %s > %s 2> %s"
            (idStr profileID)
            (show $ envPath </> "logs" </> "start.log")
            (show $ envPath </> "logs" </> "start.err")
        ]
