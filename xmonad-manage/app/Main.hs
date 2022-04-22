module Main (main) where

import Checked
import Profile
import Control.Exception
import Control.Monad.Except
import Data.Foldable
import System.Directory
import System.Environment
import System.FilePath
import System.IO
import System.Process
import Text.Printf

-- TODO Decide:
-- Git clone is Less ergonomic for change.
-- How to work with configs?

type XMonad = Executable

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
  installs <- traverse (setToExecutable . (</>) cfgDir) installScript
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

      -- Manually build profile
      ["build", profID] -> do
        printf "[build %s] Begin\n" profID
        runXM <- runXMonad <$> getExecutable "xmonad" <*> getProfile parent profID
        runXM True ["--recompile"]
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
      ProfileNotFound profID -> hPrintf stderr "Error: Profile %s not found\n" profID
      ProfileWrongFormat profID cause -> do
        hPrintf stderr "Error: Profile %s cannot be read\n" profID
        hPrintf stderr "Cause: %s\n" cause

        
