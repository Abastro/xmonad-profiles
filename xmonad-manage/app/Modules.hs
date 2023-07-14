{-# LANGUAGE OverloadedStrings #-}

module Modules (
  ModuleType (..),
  ModuleMode (..),
  ModulePath (..),
  ModuleSet (..),
  ModuleCfg (..),
  loadActiveCfg,
  activeModuleData,
  activeModules,
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
import System.Directory
import System.FilePath
import System.Process
import System.IO

-- TODO Proper builtins & More flexibility (e.g. necessary module?)

data ModuleType = X11 | Compositor | Display | Input | PolicyKit | Keyring
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
requiredTypes = S.fromList [X11, Compositor, Display]

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
loadActiveCfg mEnv@ManageEnv{..} =
  catch readCfg $ \(exc :: IOException) -> do
    logger "Cannot identify the module configuration:"
    hPrint stderr exc
    logger "Reverting configuration to default."
    createDirectoryIfMissing True (envPath </> "config")
    copyFile templatePath cfgPath
    logger "Trying again..."
    readCfg
  where
    readCfg = do
      ActiveModules{..} <- readYAMLFile userError cfgPath
      verified <- M.traverseWithKey verifyType activeSet.typedModules
      let adjusted = activeSet{typedModules = M.insert X11 (BuiltIn "x11-xmonad") verified}
      if requiredTypes `S.isSubsetOf` M.keysSet adjusted.typedModules
        then pure adjusted
        else fail "Required modules are absent." -- IDK, do I improve this error message?
        -- ? How to: Parse, not validate?
    verifyType typ modulePath = do
      ModuleCfgOf{..} <- readModuleCfg (canonPath mEnv modulePath)
      unless (moduleType == Just typ) $ do
        logger "Module %s is specified for type %s, yet it has type %s." name (show typ) (show moduleType)
        fail "Wrong module for the type."
      pure modulePath

    templatePath = envPath </> "database" </> "active-modules.yaml"
    cfgPath = envPath </> "config" </> "active-modules.yaml"

canonPath :: ManageEnv -> ModulePath -> FilePath
canonPath ManageEnv{..} = \case
  BuiltIn ident -> envPath </> "modules" </> ident
  External path -> path

activeModuleData :: ManageEnv -> ModuleSet ModulePath -> IO (ModuleSet ModuleCfg)
activeModuleData mEnv = traverse (readModuleCfg . canonPath mEnv)

-- | Load all active modules.
activeModules :: ManageEnv -> ModuleSet ModulePath -> IO [Component ModuleMode]
activeModules mEnv moduleSet = toList <$> traverse (loadModule . canonPath mEnv) moduleSet

data ModuleCfg = ModuleCfgOf
  { moduleType :: !(Maybe ModuleType)
  , name :: !T.Text
  , install :: !(Maybe FilePath)
  , run :: !FilePath
  , environment :: !(M.Map T.Text ShellString)
  , dependencies :: [Package] -- Not worth pulling in vector just for this
  }
  deriving (Show)

instance FromYAML ModuleCfg where
  parseYAML :: Node Pos -> Parser ModuleCfg
  parseYAML = withMap "module" $ \m ->
    ModuleCfgOf
      <$> (m .:! T.pack "type")
      <*> (m .: T.pack "name")
      <*> (fmap T.unpack <$> m .:! T.pack "install")
      <*> (T.unpack <$> m .: T.pack "run")
      <*> (m .:? T.pack "environment" .!= M.empty)
      <*> (m .:? T.pack "dependencies" .!= [])

readModuleCfg :: FilePath -> IO ModuleCfg
readModuleCfg moduleDir = readYAMLFile userError (moduleDir </> "module.yaml")

-- | Loads module as a comoponent, along with its type.
loadModule :: FilePath -> IO (Component ModuleMode)
loadModule moduleDir = moduleOf <$> readModuleCfg moduleDir
  where
    scriptFor path = moduleDir </> path
    moduleOf ModuleCfgOf{..} = deps <> setupEnv <> scripts
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
