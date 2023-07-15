{-# LANGUAGE OverloadedStrings #-}

module Modules (
  ModuleType (..),
  ModuleMode (..),
  ModulePath (..),
  ModuleSet (..),
  ModuleSpec (..),
  loadActiveCfg,
  activeModuleData,
  activeModules,
) where

import Common
import Component
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
import System.Process

-- TODO Proper builtins & More flexibility (e.g. necessary module?)

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

loadActiveCfg :: ManageEnv -> IO (ModuleSet ModulePath)
loadActiveCfg mEnv@ManageEnv{..} = loadConfig mEnv "active-modules.yaml" readCfg
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
      ModuleSpec{..} <- readModuleSpec (canonPath mEnv modulePath)
      unless (moduleType == Just typ) $ do
        logger "Module %s is specified for type %s, yet it has type %s." name (show typ) (show moduleType)
        fail "Wrong module for the type."
      pure modulePath

canonPath :: ManageEnv -> ModulePath -> FilePath
canonPath ManageEnv{..} = \case
  BuiltIn ident -> envPath </> "modules" </> ident
  External path -> path

activeModuleData :: ManageEnv -> ModuleSet ModulePath -> IO (ModuleSet ModuleSpec)
activeModuleData mEnv = traverse (readModuleSpec . canonPath mEnv)

-- | Load all active modules. Takes X11 module as a parameter.
activeModules :: ManageEnv -> Component ModuleMode -> ModuleSet ModulePath -> IO [Component ModuleMode]
activeModules mEnv x11Module moduleSet = do
  loaded <- traverse (loadModule . canonPath mEnv) moduleSet
  pure (x11Module : toList loaded)

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
      <$> (m .:! T.pack "type")
      <*> (m .: T.pack "name")
      <*> (fmap T.unpack <$> m .:! T.pack "install")
      <*> (T.unpack <$> m .: T.pack "run")
      <*> (m .:? T.pack "environment" .!= M.empty)
      <*> (m .:? T.pack "dependencies" .!= [])

readModuleSpec :: FilePath -> IO ModuleSpec
readModuleSpec moduleDir = readYAMLFile userError (moduleDir </> "module.yaml")

-- | Loads module as a comoponent, along with its type.
loadModule :: FilePath -> IO (Component ModuleMode)
loadModule moduleDir = moduleOf <$> readModuleSpec moduleDir
  where
    scriptFor path = moduleDir </> path
    moduleOf ModuleSpec{..} = deps <> setupEnv <> scripts
      where
        deps = ofDependencies dependencies
        scripts = fromScript (executeScript name) (scriptFor <$> install) Nothing (\Start -> scriptFor run)
        setupEnv = ofHandle (setupEnvironment name environment)

executeScript :: T.Text -> ManageEnv -> FilePath -> Context ModuleMode -> IO ()
executeScript name ManageEnv{..} script = \case
  CustomInstall -> do
    logger "[%s] Module installation using %s..." name script
    callProcess script []
  CustomRemove -> pure ()
  InvokeOn Start -> do
    logger "[%s] Setting up using %s..." name script
    callProcess script []
    logger "[%s] Setup complete." name

setupEnvironment :: T.Text -> M.Map T.Text ShellString -> ManageEnv -> Context ModuleMode -> IO ()
setupEnvironment name environment ManageEnv{..} = \case
  CustomInstall -> do
    logger "[%s] Checking shell expansions..." name
    forInEnv (logger "[Env] %s=%s")
  CustomRemove -> pure ()
  InvokeOn Start -> do
    logger "[%s] Setting up..." name
    forInEnv setServiceEnv
  where
    forInEnv act = for_ (M.toList environment) $ \(key, str) -> do
      formatted <- shellExpand str
      act (T.unpack key) (T.unpack formatted)
