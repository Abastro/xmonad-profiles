{-# LANGUAGE DerivingStrategies #-}
-- | Checked stuffs.
module Checked
  ( Executable,
    callExe,
    exeToProc,
    setToExecutable,
    getExecutable,
    ID,
    idStr,
    makeID,
    makeIDIO
  )
where

import Control.Monad
import Data.Char
import Data.Coerce
import System.Directory
import System.IO.Error
import System.Process
import Text.Printf

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

-- TODO Remove Read instance
-- | Denotes ID, made of ASCII letters w/o space
newtype ID = ID String deriving newtype (Read, Show, Eq, Ord)

idStr :: ID -> String
idStr = coerce

makeID :: String -> Maybe ID
makeID ident = ID ident <$ guard (all isAscii ident && all (not . isSpace) ident)

makeIDIO :: String -> IO ID
makeIDIO ident = maybe (ioError $ userError $ printf "Invalid ID %s" ident) pure $ makeID ident
