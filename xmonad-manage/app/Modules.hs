{-# LANGUAGE OverloadedStrings #-}

module Modules (
  ModuleType (..),
  ModuleMode (..),
  ModulePath (..),
  ModuleSet (..),
  ModuleSpec (..),
  loadActiveCfg,
  specifiedActiveModules,
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
import System.FilePath
import System.IO
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

data ModuleMode = Start
  deriving (Show, Enum, Bounded)

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

-- TODO Merge with loadModule to produce `ModuleSet (Component ModuleMode)`
loadActiveCfg :: ManageEnv -> IO (ModuleSet ModulePath)
loadActiveCfg mEnv = loadConfig mEnv "active-modules.yaml" readCfg
  where
    readCfg path = do
      ActiveModules{..} <- readYAMLFile userError path
      verified <- M.traverseWithKey verifyType activeSet.typedModules
      let adjusted = activeSet{typedModules = verified}
      if requiredTypes `S.isSubsetOf` M.keysSet adjusted.typedModules
        then pure adjusted
        else fail "Required modules are absent." -- IDK, do I improve this error message?
        -- ? How to: Parse, not validate?
    verifyType typ modulePath = do
      ModuleSpec{..} <- readModuleSpec userError (canonPath mEnv modulePath)
      unless (moduleType == Just typ) $ do
        hPrintf stderr "Module %s is specified for type %s, yet it has type %s.\n" name (show typ) (show moduleType)
        fail "Wrong module for the type."
      pure modulePath

-- ? Perhaps impleemnt overriding by user directory?
canonPath :: ManageEnv -> ModulePath -> FilePath
canonPath _ = \case
  BuiltIn ident -> localDirectory FHSConfig </> "modules" </> ident
  External path -> path

-- | Loads and gives the (list of) specified active modules.
specifiedActiveModules :: ManageEnv -> ModuleSet ModulePath -> IO (ModuleSet (Component ModuleMode))
specifiedActiveModules mEnv = traverse (loadModule . canonPath mEnv)

-- | Takes X11 module as a parameter, and combines it with rest of modules.
combineWithBuiltins :: Component ModuleMode -> ModuleSet (Component ModuleMode) -> Component ModuleMode
combineWithBuiltins x11Module moduleSet =
  withIdentifier (UnsafeMakeID "combined") $
    x11Module <> fold moduleSet

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

-- TODO IO Error handling & printing
data ModuleError
  = ModuleIOError FilePath IOError
  | ModuleWrongFormat String
  deriving (Show)
instance Exception ModuleError

readModuleSpec :: (Exception e) => (String -> e) -> FilePath -> IO ModuleSpec
readModuleSpec asException moduleDir = readYAMLFile asException (moduleDir </> "module.yaml")

loadModule :: FilePath -> IO (Component ModuleMode)
loadModule moduleDir = moduleForSpec moduleDir <$> readModuleSpec ModuleWrongFormat moduleDir

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
    printf "[%s] Module installation using %s...\n" spec.name script
    callProcess script []
  Custom Remove -> pure ()
  InvokeOn Start -> do
    printf "[%s] Setting up using %s...\n" spec.name script
    callProcess script []
    printf "[%s] Setup complete.\n" spec.name

setupEnvironment :: ModuleSpec -> ManageEnv -> Context ModuleMode -> IO ()
setupEnvironment spec _ = \case
  Custom Install -> do
    printf "[%s] Checking shell expansions...\n" spec.name
    forInEnv (printf "[Env] %s=%s\n")
  Custom Remove -> pure ()
  InvokeOn Start -> do
    printf "[%s] Setting up...\n" spec.name
    forInEnv setServiceEnv
  where
    forInEnv act = for_ (M.toList spec.environment) $ \(key, str) -> do
      act (T.unpack key) =<< shellExpand str
