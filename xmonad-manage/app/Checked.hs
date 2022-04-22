-- | Checked OS calls.
-- Assumes that executables do not get removed in the meantime.
module Checked where

import System.Directory
import System.IO.Error
import System.Process

-- | Denotes executable on PATH.
newtype Executable = Executable FilePath

callExe :: Executable -> [String] -> IO ()
callExe (Executable path) = callProcess path

exeToProc :: Executable -> [String] -> CreateProcess
exeToProc (Executable path) = proc path

setToExecutable :: FilePath -> IO Executable
setToExecutable path = do
  perm <- getPermissions path
  Executable path <$ setPermissions path (setOwnerExecutable True perm)

getExecutable :: String -> IO Executable
getExecutable exe =
  (findExecutable exe) >>= \case
    Nothing -> ioError errNotFound
    Just path -> pure (Executable path)
  where
    errNotFound = mkIOError doesNotExistErrorType "Executable does not exist in PATH" Nothing (Just exe)
