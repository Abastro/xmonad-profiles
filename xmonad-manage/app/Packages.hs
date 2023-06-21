{-# LANGUAGE DerivingStrategies #-}

module Packages (
  Distro (..),
  Package (..),
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
import Data.Maybe

newtype Distro = AsDistro T.Text
  deriving stock (Show, Eq, Ord)
  deriving newtype (FromYAML)

newtype Package = AsPackage T.Text
  deriving stock (Show, Eq, Ord)
  deriving newtype (FromYAML)

{- | Rudimentary package database.
Not a proper DB, and is never meant to be.
-}
data PkgDatabase = AsPkgDatabase
  { derivatives :: !(M.Map Distro Distro)
  , installer :: !(M.Map Distro T.Text)
  , packages :: !(M.Map Package (M.Map Distro T.Text))
  }
  deriving (Show)

instance FromYAML PkgDatabase where
  parseYAML :: Node Pos -> Parser PkgDatabase
  parseYAML = withMap "system-packages" $ \m ->
    AsPkgDatabase
      <$> (m .: T.pack "derivatives")
      <*> (m .: T.pack "installer")
      <*> (m .: T.pack "packages")

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
  got <- readExe lsbRel ["-i"]
  case stripPrefix "Distributor ID:" got of
    Nothing -> do
      logger "lsb_release failed, gave %s" got
      throwIO (UnknownDistro $ AsDistro (T.pack "invalid"))
    Just distro -> pure (AsDistro . T.strip $ T.pack distro)

withDatabase :: ManageEnv -> (PkgDatabase -> IO a) -> IO a
withDatabase ManageEnv{..} withDb = do
  pkgDatabase <- readYAMLFile DatabaseMalformed (envPath </> "database" </> "system-packages.yaml")
  withDb pkgDatabase

-- MAYBE Add e.g. Cabal-installable dependencies?
installPackages :: ManageEnv -> PkgDatabase -> Distro -> [Package] -> IO ()
installPackages ManageEnv{..} AsPkgDatabase{..} distro deps = do
  installCmd <- case installer M.!? originDistro of
    Just inst -> pure inst
    Nothing -> throwIO (UnknownDistro originDistro)
  logger "Installer command: %s" (T.unpack installCmd)

  toInstall <- case partitionEithers $ map findPackage deps of
    ([], descripts) -> pure descripts
    (errs, _) -> throwIO (PackageErrors errs)
  logger "Installing dependencies %s..." (show toInstall)

  sudo <- getExecutable "sudo"
  callExe sudo $ words (T.unpack installCmd) <> map T.unpack toInstall
 where
  originDistro = fromMaybe distro (derivatives M.!? distro)

  findPackage pkgKey = case packages M.!? pkgKey of
    Nothing -> Left (UnknownPackage pkgKey)
    Just package -> case package M.!? originDistro of
      Nothing -> Left (NotAvailableInDistro pkgKey originDistro)
      Just descript -> Right descript
