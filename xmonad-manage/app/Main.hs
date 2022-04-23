module Main (main) where

import Checked
import Control.Exception
import Control.Monad.Except
import Data.Foldable
import Data.Map.Strict qualified as M
import Data.StateVar
import Profile
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

data ManageSaved = ManageSaved
  { managePath :: !FilePath,
    profiles :: !(M.Map ID FilePath)
  }
  deriving (Read, Show)

data ManageEnv x = ManageEnv
  { envPath :: !FilePath,
    xMonad :: !x,
    logger :: forall r. PrintfType r => String -> r
  }
  deriving (Functor)

xmonadEnv :: ManageEnv () -> IO (ManageEnv XMonad)
xmonadEnv env = (<$ env) <$> getExecutable "xmonad"

varMS :: StateVar ManageSaved
varMS = dataVar "xmonad-manage" "manage-data" $ do
  managePath <- getCurrentDirectory
  pure $ ManageSaved {managePath, profiles = M.empty}

-- | Initial installation.
installInit :: Executable -> ManageEnv () -> IO ()
installInit sudo ManageEnv {logger} = do
  findExecutable "xmonad" >>= \case
    Just _ -> logger "xmonad found in PATH"
    Nothing -> callCommand "cabal install xmonad"

  callExe sudo (["apt", "install"] <> dependencies)
  where
    dependencies =
      ["xcompmgr", "suckless-tools", "gnome-screensaver", "xss-lock", "gnome-keyring"]

-- | Installs a profile - first argument is sudo.
installProfile :: Executable -> ManageEnv XMonad -> Profile -> IO ()
installProfile sudo mEnv profile@Profile {..} = do
  installs <- traverse (setToExecutable . (</>) cfgDir) installScript
  _ <- setToExecutable starter -- For now, let's set starter here
  writeFile runnerPath (runner starter)
  callExe sudo ["ln", "-sf", runnerPath, runnerLinked]

  traverse_ (`callExe` []) installs
  runXMonad mEnv profile True ["--recompile"]
  where
    ProfileCfg {profileID, profileName, installScript} = profCfg
    runnerLinked = "/usr" </> "share" </> "xsessions" </> idStr profileID <.> "desktop"
    runnerPath = dataDir </> "run" <.> "desktop"
    runner starter =
      unlines
        [ printf "[Desktop Entry]",
          printf "Encoding=UTF-8",
          printf "Name=%s" profileName,
          printf "Comment=Xmonad profile %s" (idStr profileID),
          printf "Exec=%s %s" (show starter) (idStr profileID),
          printf "Type=XSession"
        ]

-- | Runs xmonad for profile with given options.
runXMonad :: ManageEnv XMonad -> Profile -> Bool -> [String] -> IO ()
runXMonad ManageEnv {xMonad} Profile {dataDir, cfgDir, cacheDir, logDir} untilEnd opts = do
  setEnv "XMONAD_DATA_DIR" dataDir
  setEnv "XMONAD_CONFIG_DIR" cfgDir
  setEnv "XMONAD_CACHE_DIR" cacheDir
  if untilEnd
    then callExe xMonad opts
    else do
      [logs, errors] <- traverse openLog ["xmonad.log", "xmonad.err"]
      withCreateProcess (exeToProc xMonad opts) {std_out = UseHandle logs, std_err = UseHandle errors} $
        \_ _ _ ph -> () <$ waitForProcess ph
  where
    openLog name = openFile (logDir </> name) WriteMode

-- TODO Remove current directory dependency

-- | The manager program. Current directory needs to be the profile main directory.
main :: IO ()
main = do
  home <- getHomeDirectory
  (`catch` handleError) $ do
    args <- getArgs
    saved <- get varMS
    let logger str = printf (printf "[%s] %s\n" (unwords args) str)
        ManageSaved {managePath = envPath, profiles} = saved
        mEnv = ManageEnv {envPath, xMonad = (), logger}
        setupExe = envPath </> "xmonad.setup"
    sudo <- getExecutable "sudo"

    case args of
      -- Initiation run
      [] -> logger "Successfully Installed"
      -- Main installation
      ["install"] -> do
        logger "Begin"
        setToExecutable setupExe
        installInit sudo mEnv
        logger "End"

      -- Profile-specific installation
      ["install", ident] -> do
        profID <- makeIDM ident
        let cfgPath = envPath </> ident
        logger "Begin"
        join $ installProfile sudo <$> xmonadEnv mEnv <*> getProfileFromPath envPath profID cfgPath
        let update saved@ManageSaved {profiles} = saved {profiles = M.insert profID cfgPath profiles}
        varMS $~ update -- update saved
        logger "End"

      -- Manually build profile
      ["build", ident] -> do
        profID <- makeIDM ident
        cfgPath <- maybe (throwIO $ ProfileNotFound profID) pure $ profiles M.!? profID
        logger "Begin"
        runXM <- runXMonad <$> xmonadEnv mEnv <*> getProfileFromPath envPath profID cfgPath
        runXM True ["--recompile"]
        logger "End"

      -- Automatic profile run
      ["run", ident] -> do
        profID <- makeIDM ident
        cfgPath <- maybe (throwIO $ ProfileNotFound profID) pure $ profiles M.!? profID
        logger "Setup"
        withCurrentDirectory home $ callProcess setupExe []
        logger "Booting xmonad"
        runXM <- runXMonad <$> xmonadEnv mEnv <*> getProfileFromPath envPath profID cfgPath
        withCurrentDirectory home $ runXM False []
        logger "Exit"

      -- Illegal Arguments
      args -> do
        hPrintf stderr "Unexpected command %s\n" (show args)
        hPrintf stderr "Supported commands:\n"
        hPrintf stderr "install, install <profile>, build <profile> [dir], run <profile>\n"
  where
    -- MAYBE Do these need to be here?
    handleError = \case
      ProfileNotFound profID -> hPrintf stderr "Error: Profile %s not found\n" (idStr profID)
      ProfileWrongFormat profID cause -> do
        hPrintf stderr "Error: Profile %s cannot be read\n" (idStr profID)
        hPrintf stderr "Cause: %s\n" cause
