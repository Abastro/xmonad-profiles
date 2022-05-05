{-# LANGUAGE DerivingStrategies #-}

-- | Checked stuffs.
module Common
  ( Executable,
    callExe,
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
  )
where

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.ByteString.Lazy qualified as B
import Data.Char
import Data.Coerce
import Data.StateVar
import Data.YAML
import System.Directory
import System.FilePath
import System.IO
import System.IO.Error
import System.Process
import Text.Printf
import Text.Read
import qualified Data.Text as T

-- | Denotes executable on PATH.
newtype Executable = Executable FilePath
  deriving (Show)

callExe :: Executable -> [String] -> IO ()
callExe = coerce callProcess

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
dataVar :: (Read a, Show a) => String -> String -> IO a -> StateVar a
dataVar appName loc mkDef = makeStateVar load save
  where
    load = do
      msave <- datPath
      let readAsA = withFile msave ReadMode (evaluate <=< fmap readMaybe . hGetContents)
      readAsA <|> pure Nothing >>= \case
        Just saved -> pure saved
        Nothing -> do
          defVal <- mkDef
          defVal <$ writeFile msave (show defVal)
    save saved = do
      msave <- datPath
      writeFile msave (show saved)

    datPath = do
      dataDir <- getXdgDirectory XdgData appName
      createDirectoryIfMissing True dataDir
      (dataDir </> loc) <$ setPermissions dataDir perm
    perm = setOwnerSearchable True . setOwnerReadable True . setOwnerWritable True $ emptyPermissions

-- TODO Remove Read instance

-- | Denotes ID, made of ASCII letters w/o space
newtype ID = ID String deriving newtype (Read, Show, Eq, Ord)

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

readYAMLFile :: (FromYAML a, Exception e) => (IOError -> e) -> (String -> e) -> FilePath -> IO a
readYAMLFile ioErr formatErr path = do
  file <- catch @IOError (B.readFile path) $ throwIO . ioErr
  case decode1 file of
    Left (pos, err) -> (throwIO . formatErr) (prettyPosWithSource pos file "Wrong format" <> err)
    Right st -> pure st
