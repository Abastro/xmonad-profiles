{-# LANGUAGE DerivingStrategies #-}

module Packages (
  ManageID (..),
  Package (..),
  packageName,
  SystemPackages,
  PackageData,
  InstallCond (..),
  loadPackageData,
  getDatabase,
  installPackages,
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
import Text.Printf

-- | Manager ID, could be either distribution or installation medium.
newtype ManageID = ManageIDOf T.Text
  deriving stock (Show, Eq, Ord)
  deriving newtype (FromYAML)

newtype Package = AsPackage T.Text
  deriving stock (Show, Eq, Ord)
  deriving newtype (FromYAML)
packageName :: Package -> T.Text
packageName (AsPackage name) = name

-- | Rudimentary package database.
-- Not a proper DB, and is never meant to be.
data SystemPackages = SystemPackages
  { derivatives :: !(M.Map ManageID ManageID)
  , distros :: !(M.Map ManageID Installer)
  , commons :: [Manager]
  , packages :: !(M.Map Package PackageInfo)
  }
  deriving (Show)

instance FromYAML SystemPackages where
  parseYAML :: Node Pos -> Parser SystemPackages
  parseYAML = withMap "system-packages" $ \m ->
    SystemPackages
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

data PackageInfo = PackageInfo
  { components :: Maybe Components
  , naming :: Maybe (M.Map ManageID T.Text)
  }
  deriving (Show)
emptyInfo :: PackageInfo
emptyInfo = PackageInfo Nothing Nothing

-- | Components are currently only used for checking presence.
data Components = WithComponents
  { executable :: [T.Text]
  , library :: [T.Text]
  }
  deriving (Show)

instance FromYAML PackageInfo where
  parseYAML :: Node Pos -> Parser PackageInfo
  parseYAML = withMap "package" $ \m ->
    PackageInfo
      <$> (m .:? "components")
      <*> (m .:? "naming")
instance FromYAML Components where
  parseYAML :: Node Pos -> Parser Components
  parseYAML = withMap "components" $ \m ->
    WithComponents
      <$> (m .:? "executable" .!= [])
      <*> (m .:? "library" .!= [])

data PackagesError
  = DatabaseMalformed String
  | UnknownDistro ManageID
  | UnknownPackages [Package]
  | NotAvailableInDistro ManageID [Package]
  deriving (Show)

instance Exception PackagesError

data InstallCond = WhenAbsent | AlwaysInstall

-- | Aggregated data
data PackageData = PackageData
  { currentDistro :: !ManageID
  , systemPackages :: !SystemPackages
  }
  deriving (Show)

loadPackageData :: ManageEnv -> IO PackageData
loadPackageData mEnv = PackageData <$> findDistro <*> getDatabase mEnv

findDistro :: IO ManageID
findDistro = do
  got <- readProcess "lsb_release" ["-i"] []
  case stripPrefix "Distributor ID:" got of
    Nothing -> do
      printf "lsb_release failed, gave %s\n" got
      throwIO (UnknownDistro $ ManageIDOf (T.pack "invalid"))
    Just distro -> pure (ManageIDOf . T.strip $ T.pack distro)

getDatabase :: ManageEnv -> IO SystemPackages
getDatabase ManageEnv{..} = do
  readYAMLFile DatabaseMalformed (envPath </> "database" </> "system-packages.yaml")

-- ?? How did I write this ??
installPackages :: ManageEnv -> PackageData -> InstallCond -> [Package] -> IO ()
installPackages mEnv@ManageEnv{..} PackageData{..} cond deps = do
  distroInst <- case systemPackages.distros M.!? originDistro of
    Just inst -> pure inst
    Nothing -> throwIO (UnknownDistro originDistro)

  -- Locate package info.
  let pkgInfos = M.restrictKeys systemPackages.packages depSet
      missings = S.filter (`M.notMember` pkgInfos) depSet
  unless (S.null missings) $ do
    throwIO (UnknownPackages $ S.toList missings)

  -- Find out what packages are installable.
  let managers = ManagerOf originDistro distroInst : systemPackages.commons
      (missingInstalls, installTargets) = mapAccumL splitOutInstallable pkgInfos managers
  unless (M.null missingInstalls) $ do
    throwIO (NotAvailableInDistro currentDistro $ M.keys missingInstalls)

  -- Then, installs the packages.
  let installFor (ManagerOf _ installer) = installPkgsWith mEnv installer cond
  zipWithM_ installFor managers installTargets -- ? Maybe This is fragile
  where
    depSet = S.fromList deps
    originDistro = fromMaybe currentDistro (systemPackages.derivatives M.!? currentDistro)

splitOutInstallable ::
  M.Map Package PackageInfo ->
  Manager ->
  (M.Map Package PackageInfo, M.Map Package (PackageInfo, T.Text))
splitOutInstallable pkgs (ManagerOf manageID _) = M.mapEitherWithKey nameWithInfo pkgs
  where
    nameWithInfo pkg pkgInfo = (pkgInfo,) <$> packageNameOn manageID pkg pkgInfo

packageNameOn :: ManageID -> Package -> PackageInfo -> Either PackageInfo T.Text
packageNameOn manageID (AsPackage pkgID) pkgInfo = case pkgInfo.naming of
  Just names -> maybe (Left pkgInfo) Right $ names M.!? manageID
  Nothing -> Right pkgID

installPkgsWith :: ManageEnv -> Installer -> InstallCond -> M.Map Package (PackageInfo, T.Text) -> IO ()
installPkgsWith ManageEnv{..} InstallerOf{..} cond pkgs = do
  printf "Installing with: %s %s\n" (T.unpack instName) (T.unpack argInstall)
  case cond of
    WhenAbsent -> do
      (existing, needed) <- M.mapEither id <$> traverse (uncurry $ flip detectInstalled) pkgs
      let targets = M.elems needed
      printf "Already installed packages: %s\n" (show . map packageName $ M.keys existing)
      installPkgs targets
    AlwaysInstall -> do
      printf "NOTE: always-install option specified, installation includes preexisting packages.\n"
      installPkgs (snd <$> M.elems pkgs)
  where
    installPkgs [] = printf "All dependencies are installed.\n"
    installPkgs targets = do
      printf "Installing dependencies %s...\n" (show targets)
      if needRoot
        then callProcess "sudo" (map T.unpack $ instName : argInstall : targets)
        else callProcess (T.unpack instName) (map T.unpack $ argInstall : targets)

data Existing = Exists

detectInstalled :: T.Text -> PackageInfo -> IO (Either Existing T.Text)
detectInstalled target = \case
  PackageInfo{components = Just WithComponents{..}} -> do
    hasAllExes <- and <$> traverse hasExecutable executable
    hasAllLibs <- and <$> traverse hasLibrary library
    pure $ if hasAllExes && hasAllLibs then Left Exists else Right target
  _ -> pure (Right target) -- No way to detect in this case
  where
    hasExecutable exe = isJust <$> findExecutable (T.unpack exe)
    hasLibrary _ = pure False -- Currently, cannot detect libraries
