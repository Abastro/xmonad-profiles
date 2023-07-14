{-# LANGUAGE OverloadedStrings #-}

module Modules (
  ModuleSaved (..),
  ModuleMode (..),
  mkVarModS,
  builtInModules,
  moduleInfos,
  activeModules,
  loadModule,
) where

import Common
import Component
import Data.Either
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

newtype ModuleSaved = ModuleSaved [FilePath]
  deriving (Show, Generic)
instance Serialize ModuleSaved

modulePath envPath ident = envPath </> "modules" </> ident

mkVarModS :: ManageEnv -> (StateVar ModuleSaved, IO ())
mkVarModS mEnv@ManageEnv{..} = dataVar "xmonad-manage" "active-modules" $ do
  logger "Cannot load active modules, restored to default. Please run setup."
  pure . ModuleSaved $ builtInModules mEnv

builtInModules :: ManageEnv -> [FilePath]
builtInModules ManageEnv{..} =
  modulePath envPath <$> ["compositor-picom", "display-lightdm", "policykit-gnome", "input-ibus", "keyring-gnome"]

-- TODO Find a way to use Built-in modules appropriately
-- TODO More flexibility (e.g. necessary module?)
-- TODO Handle when new built-in module is added - not needed for now, but eh..
-- Migrating to DB with direct-sqlite should be first step.

data ModuleType = Compositor | Display | Input | PolicyKit | Keyring
  deriving (Show, Eq, Ord, Enum, Bounded)

data ModuleMode = Start
  deriving (Show, Enum, Bounded)

-- | Information about the given modules.
moduleInfos :: [FilePath] -> IO (M.Map (Maybe ModuleType) [(FilePath, T.Text)])
moduleInfos modules = M.fromListWith (++) . (baseline ++) <$> traverse pairedRead modules
  where
    baseline = map (, []) (Nothing : map Just [minBound .. maxBound])
    pairedRead path = do
      ModuleCfgOf{..} <- readModuleCfg path
      pure (moduleType, [(path, name)])

-- | Load all active modules.
activeModules :: ManageEnv -> ModuleSaved -> IO [Component ModuleMode]
activeModules ManageEnv{..} (ModuleSaved modules) = do
  (_, x11Module) <-  loadModule (modulePath envPath "x11-xmonad") -- Currently hard-coded.
  (others, typed) <- partitionEithers . map classify <$> traverse loadModule modules
  -- Only load the first ones; Error checking? Maybe later
  let deduped = M.elems $ M.fromList typed
  pure (x11Module : deduped <> others)
  where
    classify = \case
      (Nothing, mod) -> Left mod
      (Just typ, mod) -> Right (typ, mod)


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
loadModule :: FilePath -> IO (Maybe ModuleType, Component ModuleMode)
loadModule moduleDir = moduleOf <$> readModuleCfg moduleDir
  where
    scriptFor path = moduleDir </> path
    moduleOf ModuleCfgOf{..} = (moduleType, deps <> setupEnv <> scripts)
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
