{-# LANGUAGE DerivingStrategies #-}

-- | Checked stuffs.
module Common (
  setToExecutable,
  dataVar,
  ID,
  idStr,
  makeID,
  makeIDM,
  readYAMLFile,
  setServiceEnv,
  ServiceType (..),
  ServiceStream (..),
  Service (..),
  serviceFile,
) where

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.ByteString.Lazy qualified as B
import Data.Char
import Data.Coerce
import Data.Serialize
import Data.StateVar
import Data.Text qualified as T
import Data.YAML
import System.Directory
import System.FilePath
import System.IO
import Text.Printf
import System.Process

setToExecutable :: FilePath -> IO ()
setToExecutable path = do
  perm <- getPermissions path
  setPermissions path (setOwnerExecutable True perm)

-- | Data variable stored in XDG_DATA_DIR, with an action to reset it to default.
dataVar :: (Serialize a) => String -> String -> IO a -> (StateVar a, IO ())
dataVar appName loc mkDef = (var, restore)
  where
    var = makeStateVar load save
    restore = mkDef >>= (var $=)
    load = do
      msave <- datPath
      let readAsA = withFile msave ReadMode (evaluate <=< fmap decodeLazy . B.hGetContents)
      readAsA <|> pure (Left "") >>= \case
        Right saved -> pure saved
        Left _ -> do
          defVal <- mkDef
          defVal <$ B.writeFile msave (encodeLazy defVal)
    save saved = do
      msave <- datPath
      B.writeFile msave (encodeLazy saved)

    datPath = do
      dataDir <- getXdgDirectory XdgData appName
      createDirectoryIfMissing True dataDir
      (dataDir </> loc) <$ setPermissions dataDir perm
    perm = setOwnerSearchable True . setOwnerReadable True . setOwnerWritable True $ emptyPermissions

-- | Denotes ID, made of ASCII letters w/o space
newtype ID = ID String deriving newtype (Show, Eq, Ord, Serialize)

idStr :: ID -> String
idStr = coerce

makeID :: String -> Maybe ID
makeID ident = ID ident <$ guard (all isAscii ident && not (any isSpace ident))

makeIDM :: (MonadFail f) => String -> f ID
makeIDM ident = maybe (fail failMsg) pure $ makeID ident
  where
    failMsg = printf "ID %s contains illegal letter or spaces" ident

instance FromYAML ID where
  parseYAML :: Node Pos -> Parser ID
  parseYAML n = parseYAML n >>= makeIDM . T.unpack

readYAMLFile :: (FromYAML a, Exception e) => (String -> e) -> FilePath -> IO a
readYAMLFile formatErr path = do
  file <- B.readFile path
  case decode1 file of
    Left (pos, err) -> (throwIO . formatErr) (prettyPosWithSource pos file "Wrong format" <> err)
    Right st -> pure st

-- | Sets environment for services.
setServiceEnv :: String -> String -> IO ()
setServiceEnv name val = do
  callProcess "systemctl" ["--user", "set-environment", name <> "=" <> val]

data ServiceType = Simple | Exec
data ServiceStream = Journal | FileWrite !FilePath

-- | A service definition for systemd application.
data Service = MkService
  { serviceType :: !ServiceType
  , description :: !T.Text
  , stdOut :: !ServiceStream
  , stdErr :: !ServiceStream
  , execStart :: !FilePath
  , wantedBy :: ![FilePath]
  }

instance Show ServiceType where
  show :: ServiceType -> String
  show = \case
    Simple -> "simple"
    Exec -> "exec"
instance Show ServiceStream where
  show :: ServiceStream -> String
  show = \case
    Journal -> "journal"
    FileWrite path -> "file:" <> path

serviceFile :: Service -> String
serviceFile MkService{..} =
  unlines
    [ printf "[Unit]"
    , printf "Description=%s" description
    , printf "[Service]"
    , printf "Type=%s" (show serviceType)
    , printf "StandardOutput=%s" (show stdOut)
    , printf "StandardError=%s" (show stdErr)
    , printf "ExecStart=%s" execStart
    , printf "[Install]"
    , wantedByTxt
    ]
  where
    wantedByTxt = case wantedBy of
      [] -> ""
      xs -> printf "WantedBy=%s" (unwords xs)
