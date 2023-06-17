{-# LANGUAGE DerivingStrategies #-}

module Packages (
  Distro (..),
  Package,
  PkgDatabase,
  withDatabase,
  installPackages,
  findDistro,
) where

import Common
import Control.Exception
import Data.Either
import Data.List
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.YAML
import Manages
import System.FilePath

newtype Distro = AsDistro T.Text
  deriving stock (Show, Eq, Ord)
  deriving newtype (FromYAML)

newtype Package = AsPackage T.Text
  deriving stock (Show, Eq, Ord)
  deriving newtype (FromYAML)

-- | Rudimentary package database.
-- Not a proper DB, and is never meant to be.
data PkgDatabase = AsPkgDatabase
  { installer :: !(M.Map Distro T.Text)
  , packages :: !(M.Map Package (M.Map Distro T.Text))
  }
  deriving (Show)

instance FromYAML PkgDatabase where
  parseYAML = withMap "system-packages" $ \m ->
    AsPkgDatabase
      <$> m
      .: T.pack "installer"
      <*> m
      .: T.pack "pacakges"

data PkgsError
  = DatabaseMalformed String
  | UnknownDistro Distro
  | PackageErrors [PackageError]
  deriving (Show)

data PackageError
  = UnknownPackage Package
  | NotAvailableInDistro Package Distro
  deriving (Show)

instance Exception PkgsError

findDistro :: ManageEnv -> IO Distro
findDistro ManageEnv{..} = do
  lsbRel <- getExecutable "lsb_release"
  mayDistro <- stripPrefix "Distributor ID: " <$> readExe lsbRel ["-i"]
  case mayDistro of
    Nothing -> do
      logger "lsb_release did not return valid distro"
      throwIO (UnknownDistro $ AsDistro (T.pack "invalid"))
    Just distro -> pure (AsDistro $ T.pack distro)

withDatabase :: ManageEnv -> (PkgDatabase -> IO a) -> IO a
withDatabase ManageEnv{..} withDb = do
  pkgDatabase <- readYAMLFile DatabaseMalformed (envPath </> "database" </> "system-packages.yaml")
  withDb pkgDatabase

-- MAYBE Add e.g. Cabal-installable dependencies?
installPackages :: PkgDatabase -> Distro -> [Package] -> IO ()
installPackages AsPkgDatabase{..} distro deps = do
  installCmd <- case installer M.!? distro of
    Just inst -> pure inst
    Nothing -> throwIO (UnknownDistro distro)

  toInstall <- case partitionEithers $ map findPackage deps of
    ([], descripts) -> pure descripts
    (errs, _) -> throwIO (PackageErrors errs)

  sudo <- getExecutable "sudo"
  callExe sudo $ words (T.unpack installCmd) <> map T.unpack toInstall
  where
    findPackage pkgKey = case packages M.!? pkgKey of
      Nothing -> Left (UnknownPackage pkgKey)
      Just package -> case package M.!? distro of
        Nothing -> Left (NotAvailableInDistro pkgKey distro)
        Just descript -> Right descript
