{-# LANGUAGE OverloadedStrings #-}

module Modules (
  ModuleType (..),
  ModuleMode (..),
  ModulePath (..),
  ModuleSet (..),
  ModuleSpec (..),
  activeModules,
  combineWithBuiltins,
) where

import Common
import Component
import Control.Exception
import Control.Monad
import Data.Foldable
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Data.Text qualified as T
import Data.YAML
import GHC.Generics
import Manages
import Packages
import References
import System.FilePath
import System.Posix hiding (Start)
import System.Process
import Text.Printf

data ModuleType = Compositor | Display | Input | PolicyKit | Keyring
  deriving (Show, Eq, Ord, Enum, Bounded, Generic)

instance FromYAML ModuleType where
  parseYAML :: Node Pos -> Parser ModuleType
  parseYAML = withStr "type" $ \case
    "compositor" -> pure Compositor
    "display" -> pure Display
    "input" -> pure Input
    "policykit" -> pure PolicyKit
    "keyring" -> pure Keyring
    _ -> fail "invalid type"

requiredTypes :: S.Set ModuleType
requiredTypes = S.fromList [Compositor, Display]

data ModulePath = BuiltIn String | External FilePath
  deriving (Show, Generic)

-- | Module set parameterized by way of storing modules.
-- Could be actual modules, path of modules, or be the set of metadata.
--
-- * NOTE: Typed modules are loaded before other modules.
data ModuleSet a = ModuleSetOf
  { typedModules :: M.Map ModuleType a
  , otherModules :: [a]
  }
  deriving (Show, Functor, Foldable, Traversable, Generic)

setWithTypes :: ModuleSet a -> ModuleSet (Maybe ModuleType, a)
setWithTypes aSet =
  ModuleSetOf
    { typedModules = M.mapWithKey (\k v -> (Just k, v)) aSet.typedModules
    , otherModules = map (Nothing,) aSet.otherModules
    }

-- | Denotes the module configuration.
newtype ActiveModules = ActiveModules
  { activeSet :: ModuleSet ModulePath
  }
  deriving (Show)

instance FromYAML ModulePath where
  parseYAML :: Node Pos -> Parser ModulePath
  parseYAML = withStr "module-path" $ \case
    txt
      | Just ident <- T.stripPrefix "builtin:" txt -> pure $ BuiltIn (T.unpack ident)
      | Just path <- T.stripPrefix "external:" txt -> pure $ External (T.unpack path)
    _ -> fail "Invalid module path, need either 'builtin:' or 'external:' as prefix."

instance FromYAML ActiveModules where
  parseYAML :: Node Pos -> Parser ActiveModules
  parseYAML = withMap "active-modules" $ \m ->
    ActiveModules <$> do
      ModuleSetOf <$> (m .: "typed-modules") <*> (m .:? "other-modules" .!= [])

canonPath :: ManageEnv -> ModulePath -> FilePath
canonPath mEnv = \case
  BuiltIn ident -> mEnv.moduleDir </> ident
  External path -> path

-- | Takes X11 module as a parameter, and combines it with rest of modules.
combineWithBuiltins :: Component ModuleMode -> ModuleSet (Component ModuleMode) -> Component ModuleMode
combineWithBuiltins x11Module moduleSet =
  withIdentifier (UnsafeMakeID "combined") . withHandleWrap owning $
    x11Module <> fold moduleSet
  where
    owning innerHandle mEnv mode = case mode of
      InvokeOn _ -> innerHandle mEnv mode
      Custom _ -> do
        -- Only own the module dir on installation/removal.
        userID <- getEffectiveUserID
        ownerID <- fileOwner <$> getFileStatus mEnv.moduleDir
        if userID == ownerID
          then innerHandle mEnv mode
          else do
            -- This needs actual bracket.
            bracket_ (ownModuleDir mEnv userID) (disownModuleDir mEnv ownerID) $
              innerHandle mEnv mode

    ownModuleDir mEnv userID = do
      userEntry <- getUserEntryForID userID
      combinedLog mEnv.moduleDir $ OwnModuleDir userEntry.userName
      callProcess "sudo" ["chown", "-R", show userID, mEnv.moduleDir]

    disownModuleDir mEnv origOwnerID = do
      origOwnerEntry <- getUserEntryForID origOwnerID
      combinedLog mEnv.moduleDir $ DisownModuleDir origOwnerEntry.userName
      callProcess "sudo" ["chown", "-R", show origOwnerID, mEnv.moduleDir]

data ModuleSpec = ModuleSpec
  { moduleType :: !(Maybe ModuleType)
  , name :: !T.Text
  , install :: !(Maybe FilePath)
  , run :: !FilePath
  , environment :: !(M.Map T.Text ShellString)
  , dependencies :: [Package] -- Not worth pulling in vector just for this
  }
  deriving (Show)

instance FromYAML ModuleSpec where
  parseYAML :: Node Pos -> Parser ModuleSpec
  parseYAML = withMap "module" $ \m ->
    ModuleSpec
      <$> (m .:! "type")
      <*> (m .: "name")
      <*> (fmap T.unpack <$> m .:! "install")
      <*> (T.unpack <$> m .: "run")
      <*> (m .:? "environment" .!= M.empty)
      <*> (m .:? "dependencies" .!= [])

data ModuleError
  = ModuleIOError FilePath IOError
  | ModuleWrongFormat String
  | ActiveConfigWrongFormat String
  | ModuleTypeMismatch T.Text (Maybe ModuleType) (Maybe ModuleType)
  | RequiredModuleMissing
  deriving (Show)
instance Exception ModuleError

activeModules :: ManageEnv -> IO (ModuleSet (Component ModuleMode))
activeModules mEnv = do
  ActiveModules{..} <- readYAMLFile ActiveConfigWrongFormat (mEnv.configDir </> "active-modules.yaml")
  loaded <- traverse (uncurry $ loadModule mEnv) (setWithTypes activeSet)
  unless (requiredTypes `S.isSubsetOf` M.keysSet loaded.typedModules) $ do
    throwIO RequiredModuleMissing -- TODO Better error here
  pure loaded

-- | Tries to load a module, and throws error if not in the desired type.
loadModule :: ManageEnv -> Maybe ModuleType -> ModulePath -> IO (Component ModuleMode)
loadModule mEnv expectedTyp modulePath = wrapIOError $ do
  spec@ModuleSpec{moduleType, name} <- readYAMLFile ModuleWrongFormat (moduleDir </> "module.yaml")
  if moduleType == expectedTyp
    then throwIO $ ModuleTypeMismatch name expectedTyp moduleType
    else pure $ moduleForSpec moduleDir spec
  where
    moduleDir = canonPath mEnv modulePath
    wrapIOError = handle @IOError (throwIO . ModuleIOError moduleDir)

moduleForSpec :: FilePath -> ModuleSpec -> Component ModuleMode
moduleForSpec moduleDir spec =
  withIdentifier (UnsafeMakeID $ T.unpack spec.name) $
    mconcat
      [ ofDependencies spec.dependencies
      , ofHandle (setupEnvironment spec)
      , scripts >>> traversing_ (ofHandle $ executeScript spec)
      ]
  where
    scripts = executableScripts $ \case
      Custom Install -> scriptFor <$> spec.install
      Custom Remove -> Nothing
      InvokeOn Start -> Just (scriptFor spec.run)

    scriptFor path = moduleDir </> path

executeScript :: ModuleSpec -> FilePath -> Context ModuleMode -> IO ()
executeScript spec script = \case
  Custom Install -> do
    moduleLog spec.name $ ModuleScriptPhase (Custom Install) Pre script
    callProcess script []
  Custom Remove -> pure ()
  InvokeOn Start -> do
    moduleLog spec.name $ ModuleScriptPhase (InvokeOn Start) Pre script
    callProcess script []
    moduleLog spec.name $ ModuleScriptPhase (InvokeOn Start) Post script

setupEnvironment :: ModuleSpec -> ManageEnv -> Context ModuleMode -> IO ()
setupEnvironment spec _ = \case
  Custom Install -> do
    moduleLog spec.name ModuleCheckShellExpand
    forInEnv (printf "%s=%s\n")
  Custom Remove -> pure ()
  InvokeOn Start -> do
    moduleLog spec.name ModuleSetupEnvironment
    forInEnv setServiceEnv
  where
    forInEnv act = for_ (M.toList spec.environment) $ \(key, str) -> do
      act (T.unpack key) =<< shellExpand str
