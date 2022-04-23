{-# LANGUAGE DerivingStrategies #-}
-- | Checked stuffs.
module Checked
  ( Executable,
    callExe,
    exeToProc,
    setToExecutable,
    getExecutable,
    dataVar,
    ID,
    idStr,
    makeID,
    makeIDM
  )
where

import Control.Monad
import Data.Char
import Data.Coerce
import System.Directory
import System.IO.Error
import System.Process
import Text.Printf
import Data.StateVar
import Control.Applicative
import Text.Read
import System.FilePath

-- | Denotes executable on PATH.
newtype Executable = Executable FilePath

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
  (findExecutable exe) >>= \case
    Nothing -> ioError errNotFound
    Just path -> pure (Executable path)
  where
    errNotFound = mkIOError doesNotExistErrorType "Executable does not exist in PATH" Nothing (Just exe)

-- Not really checked, but anyway
-- | Data variable stored in XDG_DATA_DIR
dataVar :: (Read a, Show a) => String -> String -> IO a -> StateVar a
dataVar appName loc mkDef = makeStateVar load save
  where
    load = do
      msave <- datPath
      (readMaybe <$> readFile msave) <|> pure Nothing >>= \case
        Just saved -> pure saved
        Nothing -> do
          putStrLn "Manager path not yet specified, setting to current directory"
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
    failMsg = printf "ID contains illegal letter or spaces %s" ident

