{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Packages (
  ManageID (..),
  Package (..),
  PkgDatabase,
  Requirement (..),
  requireDeps,
  withDatabase,
  meetRequirements,
  findDistro,
) where

import Common
import Control.Exception
import Control.Monad
import Data.List
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Set qualified as S
import Data.Text qualified as T
import Data.YAML
import Manages
import System.Directory (findExecutable)
import System.FilePath
import System.Process

-- | Manager ID, could be either distribution or installation medium.
newtype ManageID = ManageIDOf T.Text
  deriving stock (Show, Eq, Ord)
  deriving newtype (FromYAML)

newtype Package = AsPackage T.Text
  deriving stock (Show, Eq, Ord)
  deriving newtype (FromYAML)

-- | Rudimentary package database.
-- Not a proper DB, and is never meant to be.
data PkgDatabase = AsPkgDatabase
  { derivatives :: !(M.Map ManageID ManageID)
  , distros :: !(M.Map ManageID Installer)
  , commons :: [Manager]
  , packages :: !(M.Map Package PkgInfo)
  }
  deriving (Show)

instance FromYAML PkgDatabase where
  parseYAML :: Node Pos -> Parser PkgDatabase
  parseYAML = withMap "system-packages" $ \m ->
    AsPkgDatabase
      <$> (m .: T.pack "derivatives")
      <*> (m .: T.pack "distros")
      <*> (m .: T.pack "commons")
      <*> (M.map (fromMaybe emptyInfo) <$> m .: T.pack "packages")

data Manager = ManagerOf !ManageID !Installer
  deriving (Show)

data Installer = InstallerOf
  { instName :: T.Text
  , argInstall :: T.Text
  , needRoot :: Bool
  }
  deriving (Show)

instance FromYAML Manager where
  parseYAML :: Node Pos -> Parser Manager
  parseYAML = withMap "manager" $ \m ->
    ManagerOf <$> (m .: T.pack "id") <*> (m .: T.pack "installer")

instance FromYAML Installer where
  parseYAML :: Node Pos -> Parser Installer
  parseYAML = withMap "installer" $ \m ->
    InstallerOf
      <$> (m .: T.pack "installer")
      <*> (m .: T.pack "arg-install")
      <*> (m .: T.pack "need-root")

data PkgInfo = MkPkgInfo
  { components :: Maybe Components
  , naming :: Maybe (M.Map ManageID T.Text)
  }
  deriving (Show)
emptyInfo :: PkgInfo
emptyInfo = MkPkgInfo Nothing Nothing

-- | Components are currently only used for checking presence.
data Components = WithComponents
  { executable :: [T.Text]
  , library :: [T.Text]
  }
  deriving (Show)

instance FromYAML PkgInfo where
  parseYAML :: Node Pos -> Parser PkgInfo
  parseYAML = withMap "" $ \m ->
    MkPkgInfo
      <$> (m .:? T.pack "components")
      <*> (m .:? T.pack "naming")

instance FromYAML Components where
  parseYAML :: Node Pos -> Parser Components
  parseYAML = withMap "" $ \m ->
    WithComponents
      <$> (m .:? T.pack "executable" .!= [])
      <*> (m .:? T.pack "library" .!= [])

data PkgsError
  = DatabaseMalformed String
  | UnknownDistro ManageID
  | UnknownPackages [Package]
  | NotAvailableInDistro ManageID [Package]
  deriving (Show)

instance Exception PkgsError

-- MAYBE couple with actions for start as well?
-- | Nicely packed requirements for a component.
data Requirement = MkRequirement
  { requiredDeps :: [Package]
  , customInstall :: ManageEnv -> IO ()
  }
requireDeps :: [Package] -> Requirement
requireDeps deps = MkRequirement deps mempty

instance Semigroup Requirement where
  (<>) :: Requirement -> Requirement -> Requirement
  MkRequirement deps act <> MkRequirement deps' act' = MkRequirement (deps <> deps') (act <> act')
instance Monoid Requirement where
  mempty :: Requirement
  mempty = MkRequirement mempty mempty

findDistro :: ManageEnv -> IO ManageID
findDistro ManageEnv{..} = do
  lsbRel <- getExecutable "lsb_release"
  got <- readExe lsbRel ["-i"]
  case stripPrefix "Distributor ID:" got of
    Nothing -> do
      logger "lsb_release failed, gave %s" got
      throwIO (UnknownDistro $ ManageIDOf (T.pack "invalid"))
    Just distro -> pure (ManageIDOf . T.strip $ T.pack distro)

withDatabase :: ManageEnv -> (PkgDatabase -> IO a) -> IO a
withDatabase ManageEnv{..} withDb = do
  pkgDatabase <- readYAMLFile DatabaseMalformed (envPath </> "database" </> "system-packages.yaml")
  withDb pkgDatabase

meetRequirements :: ManageEnv -> PkgDatabase -> ManageID -> Requirement -> IO ()
meetRequirements mEnv@ManageEnv{..} pkgDb distro MkRequirement{..} = do
  logger "Installing dependencies.."
  installPackages mEnv pkgDb distro requiredDeps
  logger "Running custom installation process.."
  customInstall mEnv

-- TODO Force installation regardless of presence of a pacakge?
installPackages :: ManageEnv -> PkgDatabase -> ManageID -> [Package] -> IO ()
installPackages mEnv@ManageEnv{..} AsPkgDatabase{..} distro deps = do
  distroInst <- case distros M.!? originDistro of
    Just inst -> pure inst
    Nothing -> throwIO (UnknownDistro originDistro)
  logger "Distro-specific installer command: %s %s" (T.unpack distroInst.instName) (T.unpack distroInst.argInstall)

  -- Locate package info.
  let pkgInfos = M.restrictKeys packages depSet
      missings = S.filter (`M.notMember` pkgInfos) depSet
  unless (S.null missings) $ do
    throwIO (UnknownPackages $ S.toList missings)

  -- Find out what packages are installable.
  let managers = ManagerOf originDistro distroInst : commons
      (missingInstalls, installTargets) = mapAccumL splitOutInstallable pkgInfos managers
  unless (M.null missingInstalls) $ do
    throwIO (NotAvailableInDistro distro $ M.keys missingInstalls)

  -- Then, installs the packages.
  let installFor (ManagerOf _ installer) = installPkgsWith mEnv installer
  zipWithM_ installFor managers installTargets -- MAYBE This is fragile
  where
    depSet = S.fromList deps
    originDistro = fromMaybe distro (derivatives M.!? distro)

splitOutInstallable :: M.Map Package PkgInfo -> Manager -> (M.Map Package PkgInfo, M.Map Package (PkgInfo, T.Text))
splitOutInstallable pkgs (ManagerOf manageID _) = M.mapEitherWithKey nameWithInfo pkgs
  where
    nameWithInfo pkg pkgInfo = (pkgInfo,) <$> packageNameOn manageID pkg pkgInfo

packageNameOn :: ManageID -> Package -> PkgInfo -> Either PkgInfo T.Text
packageNameOn manageID (AsPackage pkgID) pkgInfo = case pkgInfo.naming of
  Just names -> maybe (Left pkgInfo) Right $ names M.!? manageID
  Nothing -> Right pkgID

installPkgsWith :: ManageEnv -> Installer -> M.Map Package (PkgInfo, T.Text) -> IO ()
installPkgsWith ManageEnv{..} InstallerOf{..} pkgs = do
  logger "Installing with: %s %s" (T.unpack instName) (T.unpack argInstall)
  (existing, needed) <- M.mapEither id <$> traverse (uncurry $ flip detectInstalled) pkgs
  let targets = M.elems needed
  logger "Already installed packages: %s" (show $ M.keys existing)
  unless (null targets) $ do
    logger "Installing dependencies %s..." (show targets)
    if needRoot
      then callProcess "sudo" (map T.unpack $ instName : argInstall : targets)
      else callProcess (T.unpack instName) (map T.unpack $ argInstall : targets)

data Existing = Exists

detectInstalled :: T.Text -> PkgInfo -> IO (Either Existing T.Text)
detectInstalled target = \case
  MkPkgInfo{components = Just WithComponents{..}} -> do
    hasAllExes <- and <$> traverse hasExecutable executable
    hasAllLibs <- and <$> traverse hasLibrary library
    pure $ if hasAllExes && hasAllLibs then Left Exists else Right target
  _ -> pure (Right target) -- No way to detect in this case
  where
    hasExecutable exe = isJust <$> findExecutable (T.unpack exe)
    hasLibrary _ = pure False -- Currently, cannot detect libraries
