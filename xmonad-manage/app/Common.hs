{-# LANGUAGE DerivingStrategies #-}

-- | Checked stuffs.
module Common (
  Executable,
  callExe,
  readExe,
  exeToProc,
  setToExecutable,
  getExecutable,
  mayExecutable,
  dataVar,
  ID,
  idStr,
  makeID,
  makeIDM,
  readYAMLFile,
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
import GHC.IO.Exception (IOErrorType (OtherError))
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.IO.Error
import System.Process
import Text.Printf

-- | Denotes executable on PATH.
newtype Executable = Executable FilePath
  deriving (Show)

-- | Calls the executable without delegating ctrl+C.
callExe :: Executable -> [String] -> IO ()
callExe (Executable exe) args =
  withCreateProcess (proc exe args) (\_ _ _ p -> waitForProcess p) >>= \case
    ExitSuccess -> pure ()
    ExitFailure code ->
      ioError $
        mkIOError OtherError (printf "%s: received exit code %d" (unwords $ exe : args) code) Nothing Nothing

readExe :: Executable -> [String] -> IO String
readExe (Executable exe) args = do
  readProcess exe args []

exeToProc :: Executable -> [String] -> CreateProcess
exeToProc = coerce proc

setToExecutable :: FilePath -> IO Executable
setToExecutable path = do
  perm <- getPermissions path
  setPermissions path (setOwnerExecutable True perm)
  pure (Executable path)

getExecutable :: String -> IO Executable
getExecutable exe =
  findExecutable exe >>= \case
    Nothing -> ioError errNotFound
    Just path -> pure (Executable path)
  where
    errNotFound = mkIOError doesNotExistErrorType "Executable does not exist in PATH" Nothing (Just exe)

mayExecutable :: FilePath -> IO (Maybe Executable)
mayExecutable path = do
  absPath <- canonicalizePath path
  found <- findFileWith (fmap executable . getPermissions) ["/"] absPath
  pure $ coerce path <$ found -- Want symlink-included path anyway

-- | Data variable stored in XDG_DATA_DIR
dataVar :: (Serialize a) => String -> String -> IO a -> StateVar a
dataVar appName loc mkDef = makeStateVar load save
  where
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
makeID ident = ID ident <$ guard (all isAscii ident && all (not . isSpace) ident)

makeIDM :: MonadFail f => String -> f ID
makeIDM ident = maybe (fail failMsg) pure $ makeID ident
  where
    failMsg = printf "ID contains illegal letter or spaces: %s" ident

instance FromYAML ID where
  parseYAML n = parseYAML n >>= makeIDM . T.unpack

readYAMLFile :: (FromYAML a, Exception e) => (String -> e) -> FilePath -> IO a
readYAMLFile formatErr path = do
  file <- B.readFile path
  case decode1 file of
    Left (pos, err) -> (throwIO . formatErr) (prettyPosWithSource pos file "Wrong format" <> err)
    Right st -> pure st
