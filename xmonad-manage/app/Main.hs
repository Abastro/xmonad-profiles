{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}

module Main (main) where

import Control.Exception
import Control.Monad.Except
import Data.Foldable
import Data.Text qualified as T
import System.Directory
import System.Environment
import System.FilePath
import System.IO
import System.Process
import Text.Printf

-- TODO Refactor

-- | Denotes executable on PATH.
newtype Executable = Executable FilePath

type XMonad = Executable

-- | Profile ID, restricted names
type ProfileID = String

-- | Build Info
data BuildInfo = BuildInfo
  { xmonadExe :: !String,
    otherExes :: [String]
  }
  deriving (Read, Show)

-- | Profile Config
data ProfileCfg = ProfileCfg
  { profileID :: !ProfileID,
    profileName :: !T.Text,
    installScript :: !(Maybe FilePath),
    builds :: !BuildInfo
  }
  deriving (Read, Show)

-- | Profile. Requires the config path to exist.
data Profile = Profile
  { profCfg :: !ProfileCfg,
    cfgDir, dataDir, handleDir, cacheDir, logDir :: !FilePath,
    starter :: !FilePath
  }

data ManageError
  = ExeNotFound String
  | ProfileNotFound ProfileID
  | ProfileWrongFormat ProfileID String
  deriving (Show)

instance Exception ManageError

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
getExecutable :: String -> IO Executable
getExecutable exe =
  (findExecutable exe) >>= \case
    Nothing -> throwIO (ExeNotFound exe)
    Just path -> pure (Executable path)

-- | Gets a profile, or gives Nothing if not found.
getProfile :: FilePath -> ProfileID -> IO Profile
getProfile parent profID = do
  doesDirectoryExist cfgDir >>= (`unless` throwIO (ProfileNotFound profID))
  cfgStr <- readFile (cfgDir </> "profile.cfg")
  profCfg <- case reads @ProfileCfg cfgStr of
    [(cfg, "")] -> pure cfg -- TODO Use proper parser combinators
    remaining -> throwIO (ProfileWrongFormat profID $ errMsg $ show remaining)
  pure (Profile {profCfg, cfgDir, dataDir, handleDir, cacheDir, logDir, starter})
  where
    errMsg = printf "left while parsing: %s"
    dataDir = parent </> "data" </> profID
    cfgDir = parent </> profID
    handleDir = parent </> "handle" </> profID
    cacheDir = parent </> "cache" </> profID
    logDir = parent </> "logs" </> profID
    starter = parent </> "start.sh"

-- | Initial installation.
installInit :: Executable -> IO ()
installInit sudo = do
  findExecutable "xmonad" >>= \case
    Just _ -> putStrLn "[install] xmonad found in PATH"
    Nothing -> callCommand "cabal install xmonad"

  callExe sudo (["apt", "install"] <> dependencies)
  where
    dependencies =
      ["xcompmgr", "suckless-tools", "gnome-screensaver", "xss-lock", "gnome-keyring"]

-- | Installs a profile - first argument is sudo.
installProfile :: Executable -> XMonad -> Profile -> IO ()
installProfile sudo xmonad profile@Profile {..} = do
  traverse_ (createDirectoryIfMissing True) [dataDir, handleDir, cacheDir, logDir]
  installs <- traverse (setToExecutable . (cfgDir </>)) installScript
  -- _ <- setToExecutable (cfgDir </> "build")
  _ <- setToExecutable starter -- For now, let's set starter here

  curDir <- getCurrentDirectory
  writeFile (cfgDir </> "build") (builder curDir)
  _ <- setToExecutable (cfgDir </> "build")

  writeFile runnerPath (runner starter)
  callExe sudo ["ln", "-sf", runnerPath, runnerLinked]

  traverse_ (`callExe` []) installs
  runXMonad xmonad profile True ["--recompile"]
  where
    ProfileCfg {profileID, profileName, installScript} = profCfg
    runnerLinked = "/usr" </> "share" </> "xsessions" </> profileID <.> "desktop"
    runnerPath = dataDir </> "run" <.> "desktop"
    runner starter =
      unlines
        [ printf "[Desktop Entry]",
          printf "Encoding=UTF-8",
          printf "Name=%s" profileName,
          printf "Comment=Xmonad profile %s" profileID,
          printf "Exec=%s %s" (show starter) profileID,
          printf "Type=XSession"
        ]

    builder curDir =
      unlines
        [ printf "#!/bin/sh",
          printf "cd %s || exit" (show curDir),
          printf "exec xmonad-manage build %s %s" profileID (show "$1")
        ]

-- | Builds certain profile, called by a build script.
buildProfile :: FilePath -> Profile -> IO ()
buildProfile xmonadDest Profile {profCfg, cfgDir, cacheDir} = withCurrentDirectory cfgDir $ do
  printf "[build] Building config from directory %s\n" cfgDir
  printf "[build] XMonad config destination %s\n" xmonadDest

  cabal <- getExecutable "cabal"
  -- Run build to check if it is updated
  need <- withCreateProcess (exeToProc cabal $ "build" : targets) {std_out = CreatePipe} $
    \_ (Just hout) _ _ -> do -- Let's assume stdout close = build end
      hGetContents hout >>= \l -> ("Up to date" `notElem` lines l) <$ putStr l

  -- If files are rebuilt, install it
  when need $ do
    ln <- getExecutable "ln"
    callExe cabal ("install" : targets <> installOpts)
    callExe ln ["-sf", cacheDir </> xmonadExe, xmonadDest]
    callExe cabal ("build" : targets) -- Hack to reduce checking time
  where
    ProfileCfg {builds = BuildInfo {xmonadExe, otherExes}} = profCfg
    targets = map ("exe:" <>) (xmonadExe : otherExes)
    installOpts =
      [ printf "--installdir=%s" cacheDir,
        printf "--install-method=copy",
        printf "--overwrite-policy=always"
      ]

-- | Runs xmonad for profile with given options.
runXMonad :: XMonad -> Profile -> Bool -> [String] -> IO ()
runXMonad xmonad Profile {dataDir, cfgDir, cacheDir, logDir} untilEnd opts = do
  setEnv "XMONAD_DATA_DIR" dataDir
  setEnv "XMONAD_CONFIG_DIR" cfgDir
  setEnv "XMONAD_CACHE_DIR" cacheDir
  if untilEnd
    then callExe xmonad opts
    else do
      [logs, errors] <- traverse openLog ["xmonad.log", "xmonad.err"]
      withCreateProcess (exeToProc xmonad opts) {std_out = UseHandle logs, std_err = UseHandle errors} $
        \_ _ _ ph -> () <$ waitForProcess ph
  where
    openLog name = openFile (logDir </> name) WriteMode

-- TODO Remove current directory dependency

-- | The manager program. Current directory needs to be the profile main directory.
main :: IO ()
main = do
  home <- getHomeDirectory
  parent <- getCurrentDirectory
  let setupExe = parent </> "xmonad.setup"
  (`catch` handleError) $ do
    sudo <- getExecutable "sudo"
    getArgs >>= \case
      -- Main installation
      ["install"] -> do
        printf "[install] Begin\n"
        setToExecutable setupExe
        installInit sudo
        printf "[install] End\n"

      -- Profile-specific installation
      ["install", profID] -> do
        printf "[install %s] Begin\n" profID
        join $ installProfile sudo <$> getExecutable "xmonad" <*> getProfile parent profID
        printf "[install %s] End\n" profID

      -- Automatic profile build
      ["build", profID, destFile] -> do
        printf "[build %s %s] Begin\n" profID destFile
        join $ buildProfile destFile <$> getProfile parent profID
        printf "[build %s %s] End\n" profID destFile
      -- Manual profile build
      ["build", profID] -> do -- TODO Use binfilename instead (arch-os)
        printf "[build %s] Begin\n" profID
        profile@Profile {cacheDir} <- getProfile parent profID
        buildProfile (cacheDir </> "xmonad-x86_64-linux") profile
        printf "[build %s] End\n" profID

      -- Automatic profile run
      ["run", profID] -> do
        printf "[run %s] Setup\n" profID
        withCurrentDirectory home $ callProcess setupExe []
        printf "[run %s] Booting xmonad\n" profID
        runXM <- runXMonad <$> getExecutable "xmonad" <*> getProfile parent profID
        withCurrentDirectory home $ runXM False []
        printf "[run %s] Exit" profID

      -- Illegal Arguments
      args -> do
        hPrintf stderr "Unexpected command %s\n" (show args)
        hPrintf stderr "Supported commands:\n"
        hPrintf stderr "install, install <profile>, build <profile> [dir], run <profile>\n"
  where
    -- MAYBE Do these need to be here?
    handleError = \case
      ExeNotFound exe -> hPrintf stderr "Error: Executable %s not found in PATH\n" exe
      ProfileNotFound profID -> hPrintf stderr "Error: Profile %s not found\n" profID
      ProfileWrongFormat profID cause -> do
        hPrintf stderr "Error: Profile %s cannot be read\n" profID
        hPrintf stderr "Cause: %s\n" cause
        
        
