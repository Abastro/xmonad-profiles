-- | References for states and logs.
module References (
  PrePost (..),
  ProfileMode (..),
  ProfileLog (..),
  profileLog,
  ModuleCombinedLog (..),
  combinedLog,
  ModuleMode (..),
  ModuleLog (..),
  moduleLog,
)
where

import Common
import Component
import Data.Text qualified as T
import Text.Printf

data PrePost = Pre | Post
  deriving (Show, Eq, Enum, Bounded)

prefixed :: (PrintfType r) => T.Text -> String -> r
prefixed prefix str = printf ("[" <> T.unpack prefix <> "] " <> str)

data ProfileMode = BuildMode | RunMode
  deriving (Show, Eq, Enum, Bounded)

data ProfileLog
  = ProfileScriptPhase !(Context ProfileMode) !FilePath
  | PrepareDirectories !SetupPhase
  | PrepareSession !SetupPhase
  | PrepareServices !SetupPhase
  | InstallService !SetupPhase !String
  | RunMainService !String
  deriving (Show)

profileLog :: T.Text -> ProfileLog -> IO ()
profileLog name = \case
  -- Scripts
  ProfileScriptPhase (Custom Install) script ->
    logger "Install using %s...\n" script
  ProfileScriptPhase (Custom Remove) script ->
    logger "Remove using %s...\n" script
  ProfileScriptPhase (InvokeOn BuildMode) script ->
    logger "Build through script %s...\n" script
  --
  PrepareDirectories Install ->
    logger "Preparing profile directories...\n"
  PrepareDirectories Remove ->
    logger "Removing profile directories...\n"
  --
  PrepareSession Install ->
    logger "Installing xsession desktop entry...\n"
  PrepareSession Remove ->
    logger "Removing xsession desktop entry...\n"
  --
  PrepareServices Install ->
    logger "Installing systemd services...\n"
  PrepareServices Remove ->
    logger "Removing systemd services...\n"
  --
  InstallService Install serviceName ->
    logger "Service %s being installed.\n" serviceName
  InstallService Remove serviceName ->
    logger "Service %s being removed.\n" serviceName
  _ -> pure ()
  where
    logger :: (PrintfType r) => String -> r
    logger = prefixed name

data ModuleCombinedLog = OwnModuleDir !String | DisownModuleDir !String
  deriving (Show)

combinedLog :: FilePath -> ModuleCombinedLog -> IO ()
combinedLog moduleDir = \case
  OwnModuleDir userName -> do
    printf "To set up modules, %s need to be modified.\n" moduleDir
    printf "In the setup process, %s will be temporarily owned by %s.\n" moduleDir userName
    printf "Asking for sudo permission for this operation...\n"
  DisownModuleDir origOwnerName -> do
    printf "Returning ownership of %s to the original owner %s...\n" moduleDir origOwnerName

data ModuleMode = Start
  deriving (Show, Enum, Bounded)

data ModuleLog
  = ModuleScriptPhase !(Context ModuleMode) !PrePost !FilePath
  | ModuleCheckShellExpand
  | ModuleSetupEnvironment
  deriving (Show)

moduleLog :: T.Text -> ModuleLog -> IO ()
moduleLog name = \case
  ModuleScriptPhase (Custom Install) Pre script ->
    logger "Module installation using %s...\n" script
  --
  ModuleScriptPhase (InvokeOn Start) Pre script ->
    logger "Setting up using %s...\n" script
  ModuleScriptPhase (InvokeOn Start) Post _ ->
    logger "Setup complete.\n"
  --
  ModuleCheckShellExpand ->
    logger "Checking shell expansions...\n"
  ModuleSetupEnvironment ->
    logger "Setting up...\n"
  _ -> pure ()
  where
    logger :: (PrintfType r) => String -> r
    logger = prefixed name
