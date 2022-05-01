module Main (main) where

import Checked
import Control.Exception
import Control.Monad.Except
import Data.Foldable
import Data.Map.Strict qualified as M
import Data.StateVar
import Data.Text qualified as T
import Profile
import Startup
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.Process
import Text.Printf

-- TODO Decide:
-- Git clone is Less ergonomic for change.
-- How to work with configs?

-- Startup is manually switched. For now.
-- TODO Refactor ManageSaved out
-- TODO Read/Show is not flexible enough; enable incremental read/show
data ManageSaved = ManageSaved
  { managePath :: !FilePath,
    profiles :: !(M.Map ID FilePath),
    startupDir :: !FilePath
  }
  deriving (Read, Show)

data ManageEnv = ManageEnv
  { envPath :: !FilePath,
    logger :: forall r. PrintfType r => String -> r
  }

varMS :: StateVar ManageSaved
varMS = dataVar "xmonad-manage" "manage-data" $ do
  putStrLn "Manager path not yet specified, setting to current directory"
  managePath <- getCurrentDirectory
  pure $ ManageSaved {managePath, profiles = M.empty, startupDir = managePath </> "start-basic"}

-- | Initial installation.
installInit :: ManageEnv -> Startup -> IO ()
installInit ManageEnv {logger} Startup {..} = do
  findExecutable "xmonad" >>= \case
    Just _ -> logger "xmonad found in PATH"
    Nothing -> callCommand "cabal install xmonad"

  [reqs, _] <- traverse setToExecutable [startInstall, startRun]
  callExe reqs []

-- | Installs a profile - first argument is sudo.
installProfile :: Executable -> ManageEnv -> Profile -> IO ()
installProfile sudo mEnv profile@Profile {..} = do
  writeFile startPath startScript
  _ <- setToExecutable startPath

  writeFile runnerPath runner
  callExe sudo ["ln", "-sf", runnerPath, runnerLinked]

  traverse_ (`callExe` []) profInstall
  runXMonad mEnv profile True ["--recompile"]
  where
    runnerLinked = "/usr" </> "share" </> "xsessions" </> idStr profID <.> "desktop"
    runnerPath = dataDir </> "run" <.> "desktop"
    runner =
      unlines
        [ printf "[Desktop Entry]",
          printf "Encoding=UTF-8",
          printf "Name=%s" profName,
          printf "Comment=Xmonad profile %s" (idStr profID),
          printf "Exec=%s" startPath,
          printf "Type=XSession"
        ]

    startPath = dataDir </> "starter.sh"
    startScript =
      unlines
        [ printf "#!/bin/sh",
          printf "export PATH=$HOME/.cabal/bin:$HOME/.ghcup/bin:$PATH",
          printf
            "exec xmonad-manage run %s > %s 2> %s"
            (idStr profID)
            (show $ logDir </> "start.log")
            (show $ logDir </> "start.err")
        ]

-- | Runs xmonad for profile with given options.
runXMonad :: ManageEnv -> Profile -> Bool -> [String] -> IO ()
runXMonad ManageEnv {logger} Profile {xmonadExe, dataDir, cfgDir, cacheDir, logDir} untilEnd opts = do
  setEnv "XMONAD_DATA_DIR" dataDir
  setEnv "XMONAD_CONFIG_DIR" cfgDir
  setEnv "XMONAD_CACHE_DIR" cacheDir
  logger "Running xmonad through %s" (show xmonadExe)
  if untilEnd
    then callExe xmonadExe opts
    else do
      [logs, errors] <- traverse openLog ["xmonad.log", "xmonad.err"]
      withCreateProcess (exeToProc xmonadExe opts) {std_out = UseHandle logs, std_err = UseHandle errors} $
        \_ _ _ ph -> () <$ waitForProcess ph
  where
    openLog name = openFile (logDir </> name) WriteMode

-- | The manager program. Current directory needs to be the profile main directory.
main :: IO ()
main = do
  home <- getHomeDirectory
  (`catch` handleError) $ do
    args <- getArgs
    saved <- get varMS
    let logger str = printf (printf "[%s] %s\n" (unwords args) str)
        ManageSaved {managePath = envPath, profiles, startupDir} = saved
        mEnv = ManageEnv {envPath, logger}
        getProfile profID =
          maybe (throwIO $ ProfileNotFound $ Right profID) pure $ profiles M.!? profID

    -- MAYBE Use optparse-applicative
    case args of
      -- Initiation run
      [] -> logger "Successfully Installed"
      -- Main installation
      ["install"] -> do
        logger "Begin"
        join $ installInit mEnv <$> getStartup startupDir
        logger "End"

      -- Profile-specific installation
      ["install", rawPath] -> do
        logger "Begin"
        cfgPath <- canonicalizePath rawPath
        profile@Profile {profID} <- getProfileFromPath envPath cfgPath
        join $ installProfile <$> getExecutable "sudo" <*> pure mEnv <*> pure profile
        let addProfile = M.insert profID cfgPath
        -- update saved
        varMS $~ \saved@ManageSaved {profiles} -> saved {profiles = addProfile profiles}
        logger "End"

      -- Manually build profile
      ["build", ident] -> do
        profID <- makeIDM ident
        cfgPath <- getProfile profID
        logger "Begin"
        runXM <- runXMonad mEnv <$> getProfileFromPath envPath cfgPath
        runXM True ["--recompile"]
        logger "End"

      -- Automatic profile run
      ["run", ident] -> do
        profID <- makeIDM ident
        cfgPath <- getProfile profID
        logger "Setup"
        withCurrentDirectory home $ do
          Startup {..} <- getStartup startupDir
          callProcess startRun []
          traverse_ (uncurry setEnv) (M.toList $ T.unpack <$> startEnv)
        logger "Booting xmonad"
        runXM <- runXMonad mEnv <$> getProfileFromPath envPath cfgPath
        withCurrentDirectory home $ runXM False []
        logger "Exit"

      -- Change startup
      ["startup", rawPath] -> do
        logger "Begin"
        startupDir <- canonicalizePath rawPath
        getStartup startupDir -- Checks if startup directory is valid
        varMS $~ \saved -> saved {startupDir}
        logger "Startup manage directory set to %s" startupDir
        logger "End"

      -- Illegal Arguments
      args -> do
        hPrintf stderr "Unexpected command %s\n" (show args)
        hPrintf stderr "Supported commands:\n"
        hPrintf stderr "install, install <profile>, build <profile> [dir], run <profile>\n"
  where
    -- MAYBE Do these need to be here?
    handleError = \case
      ProfileNotFound loc -> do
        case loc of
          Left profPath -> hPrintf stderr "Error: Profile not found in path %s\n" profPath
          Right profID -> hPrintf stderr "Error: Profile %s not found\n" (idStr profID)
        exitWith (ExitFailure 1)
      ProfileIOError profPath err -> do
        hPrintf stderr "Error: IO Exception while reading profile.cfg in path %s\n" profPath
        hPrintf stderr "Details: %s\n" (show err)
        exitWith (ExitFailure 2)
      ProfileWrongFormat details -> do
        hPrintf stderr "Error: Profile cannot be read from profile.cfg\n"
        hPrintf stderr "Details: %s\n" details
        exitWith (ExitFailure 3)
