{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Monad.Except
import Data.Foldable
import System.Directory
import System.Environment
import System.FilePath
import System.IO
import System.Process
import Text.Printf

-- | Denotes executable on PATH.
newtype Executable = Executable FilePath

type XMonad = Executable

type ProfileID = String

-- | Profile. Requires the config path to exist.
data Profile = Profile
  { profID :: !ProfileID,
    dataDir :: !FilePath,
    cfgDir :: !FilePath,
    cacheDir :: !FilePath,
    logDir :: !FilePath,
    starter :: !FilePath
  }

data ManageError
  = ExeNotFound String
  | ProfileNotFound ProfileID
  | IllegalArgs [String]

type ManageStack = ExceptT ManageError IO

-- | Calls the executable.
callExe :: Executable -> [String] -> IO ()
callExe (Executable path) = callProcess path

-- | Converts executable to a createprocess.
exeToProc :: Executable -> [String] -> CreateProcess
exeToProc (Executable path) = proc path

-- | Set certain file to executable
setToExecutable :: FilePath -> IO Executable
setToExecutable path = do
  perm <- getPermissions path
  Executable path <$ setPermissions path (setOwnerExecutable True perm)

-- | Gets an executable.
getExecutable :: String -> ManageStack Executable
getExecutable exe =
  liftIO (findExecutable exe) >>= \case
    Nothing -> throwError (ExeNotFound exe)
    Just path -> pure (Executable path)

-- | Gets a profile, or gives Nothing if not found.
getProfile :: FilePath -> ProfileID -> ManageStack Profile
getProfile parent profID = do
  liftIO (doesDirectoryExist cfgDir) >>= (`unless` throwError (ProfileNotFound profID))
  pure (Profile {profID, dataDir, cfgDir, cacheDir, logDir, starter})
  where
    dataDir = parent </> "data" </> profID
    cfgDir = parent </> profID
    cacheDir = parent </> "cache" </> profID
    logDir = parent </> "logs" </> profID
    starter = parent </> "start.sh"

-- | Initial installation.
installInit :: Executable -> IO ()
installInit sudo = do
  printf "[Main] Install Begin\n"

  findExecutable "xmonad" >>= \case
    Just _ -> putStrLn "[Main] xmonad found in PATH"
    Nothing -> callCommand "cabal install xmonad"

  callExe sudo (["apt", "install"] <> dependencies)

  printf "[Main] Install End\n"
  where
    dependencies =
      ["xcompmgr", "suckless-tools", "gnome-screensaver", "xss-lock", "gnome-keyring"]

-- | Installs a profile - first argument is sudo.
installProfile :: Executable -> XMonad -> Profile -> IO ()
installProfile sudo xmonad profile@Profile {..} = do
  printf "[%s] Install Begin\n" profID

  traverse_ (createDirectoryIfMissing True) [dataDir, cacheDir, logDir]
  installs <- setToExecutable (cfgDir </> "install")
  _ <- setToExecutable (cfgDir </> "build")
  _ <- setToExecutable starter -- For now, let's set starter here

  writeFile runnerPath (runner starter)
  callExe sudo ["ln", "-sf", runnerPath, runnerLinked]

  callExe installs []
  runXMonad xmonad profile True ["--recompile"]

  printf "[%s] Install End\n" profID
  where
    runnerLinked = "/usr" </> "share" </> "xsessions" </> profID <.> "desktop"
    runnerPath = dataDir </> "run" <.> "desktop"
    [logs, errors] = (logDir </>) <$> ["common.log", "common.err"]
    runner starter =
      unlines
        [ printf "[Desktop Entry]",
          printf "Encoding=UTF-8",
          printf "Name=%s" profID,
          printf "Comment=Xmonad profile <%s>" profID,
          printf "Exec=%s %s > %s 2> %s" (show starter) profID (show logs) (show errors),
          printf "Type=XSession"
        ]

-- | Runs xmonad for profile with given options.
runXMonad :: XMonad -> Profile -> Bool -> [String] -> IO ()
runXMonad xmonad Profile {..} untilEnd opts = do
  setEnv "XMONAD_DATA_DIR" dataDir
  setEnv "XMONAD_CONFIG_DIR" cfgDir
  setEnv "XMONAD_CACHE_DIR" cacheDir
  if untilEnd
    then callExe xmonad opts
    else do
      [logs, errors] <- traverse openLog ["xmonad.log", "xmonad.err"]
      () <$ createProcess (exeToProc xmonad opts) {std_out = UseHandle logs, std_err = UseHandle errors}
  where
    openLog name = openFile (logDir </> name) WriteMode

-- | The manager program. Current directory needs to be the parent.
main :: IO ()
main = do
  parent <- getCurrentDirectory
  let setupExe = parent </> "xmonad.setup"
  res <- runExceptT $ do
    sudo <- getExecutable "sudo"
    liftIO getArgs >>= \case
      ["install"] -> liftIO $ do
        setToExecutable setupExe
        installInit sudo
      ["install", profID] -> do
        inst <- installProfile sudo <$> getExecutable "xmonad" <*> getProfile parent profID
        liftIO inst
      ["run", profID] -> do
        liftIO (callProcess setupExe [])
        runXM <- runXMonad <$> getExecutable "xmonad" <*> getProfile parent profID
        liftIO (runXM False [])
      args -> throwError (IllegalArgs args)

  case res of
    Right () -> pure ()
    Left (ExeNotFound exe) -> hPrintf stderr "Error: Executable %s not found in PATH\n" exe
    Left (ProfileNotFound profID) -> hPrintf stderr "Error: Profile %s not found\n" profID
    Left (IllegalArgs args) -> do
      hPrintf stderr "Unexpected command %s\n" (show args)
      hPrintf stderr "Supported: install, install <profile>, run <profile>\n"
