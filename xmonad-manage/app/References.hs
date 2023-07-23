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
) where

import Common
import Component
import Data.Text qualified as T
import Text.Printf

data PrePost = Pre | Post
  deriving (Show, Eq, Enum, Bounded)

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
    printf "[%s] Install using %s...\n" name script
  ProfileScriptPhase (Custom Remove) script ->
    printf "[%s] Remove using %s...\n" name script
  ProfileScriptPhase (InvokeOn BuildMode) script ->
    printf "[%s] Build through script %s...\n" name script
  --
  PrepareDirectories Install ->
    printf "[%s] Preparing profile directories...\n" name
  PrepareDirectories Remove ->
    printf "[%s] Removing profile directories...\n" name
  --
  PrepareSession Install ->
    printf "[%s] Installing xsession desktop entry...\n" name
  PrepareSession Remove ->
    printf "[%s] Removing xsession desktop entry...\n" name
  --
  PrepareServices Install ->
    printf "[%s] Installing systemd services...\n" name
  PrepareServices Remove ->
    printf "[%s] Removing systemd services...\n" name
  --
  InstallService Install serviceName ->
    printf "[%s] Service %s being installed.\n" name serviceName
  InstallService Remove serviceName ->
    printf "[%s] Service %s being removed.\n" name serviceName
  _ -> pure ()

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
    printf "[%s] Module installation using %s...\n" name script
  -- Setting up on Start
  ModuleScriptPhase (InvokeOn Start) Pre script ->
    printf "[%s] Setting up using %s...\n" name script
  ModuleScriptPhase (InvokeOn Start) Post _ ->
    printf "[%s] Setup complete.\n" name
  -- Shell environments
  ModuleCheckShellExpand ->
    printf "[%s] Checking shell expansions...\n" name
  ModuleSetupEnvironment ->
    printf "[%s] Setting up...\n" name
  _ -> pure ()
