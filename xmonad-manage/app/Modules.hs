{-# LANGUAGE OverloadedStrings #-}

module Modules (
  ModuleSaved (..),
  ModuleType (..),
  ModuleMode (..),
  ModuleSet (..),
  ModuleCfg (..),
  mkVarModule,
  builtInForType,
  activeModuleCfgs,
  activeModules,
) where

import Common
import Component
import Data.Foldable
import Data.Map.Strict qualified as M
import Data.Serialize (Serialize)
import Data.StateVar
import Data.Text qualified as T
import Data.YAML
import GHC.Generics
import Manages
import Packages
import System.FilePath
import System.Process

builtInModules :: M.Map ModuleType [String]
builtInModules =
  M.fromList
    [ (X11, ["x11-xmonad"])
    , (Compositor, ["compositor-picom"])
    , (Display, ["display-lightdm", "display-sddm"])
    , (PolicyKit, ["policykit-gnome", "policykit-kde"])
    , (Keyring, ["keyring-gnome"])
    , (Input, ["input-ibus"])
    ]

-- TODO Find a way to use Built-in modules appropriately
-- TODO More flexibility (e.g. necessary module?)
-- TODO Migrating to DB with direct-sqlite, and then enable proper upgrading.

data ModuleType = X11 | Compositor | Display | Input | PolicyKit | Keyring
  deriving (Show, Eq, Ord, Enum, Bounded, Generic)

data ModuleMode = Start
  deriving (Show, Enum, Bounded)

-- | Whether a module is required for a type.
-- Only used for nwe configuration as one may preserve existing setup.
data RequiredType = Fixed | Required | Optional
  deriving (Show)

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

instance Serialize ModuleType
instance Serialize ModulePath
instance (Serialize a) => Serialize (ModuleSet a)

-- | Save data for modules.
newtype ModuleSaved = ModuleSaved
  { moduleSet :: ModuleSet ModulePath
  }
  deriving (Show, Generic)

instance Serialize ModuleSaved

mkVarModule :: ManageEnv -> (StateVar ModuleSaved, IO ())
mkVarModule _mEnv@ManageEnv{..} = dataVar "xmonad-manage" "active-modules" $ do
  logger "Cannot load active modules, restored to default. Please run setup."
  pure . ModuleSaved $ ModuleSetOf{typedModules = defModules, otherModules = []}
  where
    defModules = M.mapMaybe selectFst builtInModules
    selectFst = \case
      [] -> Nothing
      name : _ -> Just (BuiltIn name)

canonPath :: ManageEnv -> ModulePath -> FilePath
canonPath ManageEnv{..} = \case
  BuiltIn ident -> envPath </> "modules" </> ident
  External path -> path

moduleCfgs :: (Traversable t) => ManageEnv -> t ModulePath -> IO (t ModuleCfg)
moduleCfgs mEnv = traverse (readModuleCfg . canonPath mEnv)

builtInForType :: ManageEnv -> ModuleType -> IO [ModuleCfg]
builtInForType mEnv moduleType = moduleCfgs mEnv (BuiltIn <$> builtInModules M.! moduleType)

activeModuleCfgs :: ManageEnv -> ModuleSaved -> IO (ModuleSet ModuleCfg)
activeModuleCfgs mEnv saved = moduleCfgs mEnv saved.moduleSet

-- | Load all active modules.
activeModules :: ManageEnv -> ModuleSaved -> IO [Component ModuleMode]
activeModules mEnv saved = toList <$> traverse (loadModule . canonPath mEnv) saved.moduleSet

data ModuleCfg = ModuleCfgOf
  { moduleType :: !(Maybe ModuleType)
  , name :: !T.Text
  , install :: !(Maybe FilePath)
  , run :: !FilePath
  , environment :: !(M.Map T.Text ShellString)
  , dependencies :: [Package] -- Not worth pulling in vector just for this
  }
  deriving (Show)

instance FromYAML ModuleType where
  parseYAML :: Node Pos -> Parser ModuleType
  parseYAML = withStr "type" $ \case
    "compositor" -> pure Compositor
    "display" -> pure Display
    "input" -> pure Input
    "policykit" -> pure PolicyKit
    "keyring" -> pure Keyring
    _ -> fail "invalid type"

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
