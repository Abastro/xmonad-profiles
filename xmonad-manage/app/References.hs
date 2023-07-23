-- | References for most of the logs.
module References (
  ModuleCombinedLog (..),
  combinedLog,
) where

import Common
import Component
import Data.Text qualified as T
import Text.Printf

data PrePost = Pre | Post
  deriving (Show)
data ScriptPhase a = ScriptPhase !(Context a) !PrePost
  deriving (Show)

data ModuleCombinedLog = OwnModuleDir !FilePath !String | DisownModuleDir !FilePath !String
  deriving (Show)
combinedLog :: ModuleCombinedLog -> IO ()
combinedLog = \case
  OwnModuleDir moduleDir userName -> do
    printf "To set up modules, %s need to be modified.\n" moduleDir
    printf "In the setup process, %s will be temporarily owned by %s.\n" moduleDir userName
    printf "Asking for sudo permission for this operation...\n"
  DisownModuleDir moduleDir origOwnerName -> do
    printf "Returning ownership of %s to the original owner %s...\n" moduleDir origOwnerName

data ModuleLog = ModuleLog !T.Text !ModuleLogDetail
  deriving (Show)
data ModuleLogDetail
  = ModuleScriptPhase !(ScriptPhase ())
  | ModuleCheckShellExpand
  | ModuleSetupEnvironment
  deriving (Show)
