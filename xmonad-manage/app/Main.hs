module Main (main) where

import Common
import Control.Exception
import Data.Foldable
import Data.Map.Strict qualified as M
import Data.StateVar
import Manages
import Options.Applicative qualified as Opts
import Profile
import Startup
import System.Directory
import System.Environment
import System.Exit
import System.IO
import System.Process
import Text.Printf

-- TODO Decide:
-- Git clone is Less ergonomic for change.
-- How to work with configs?

-- TODO Read/Show is not flexible enough; enable incremental read/show

-- TODO Versioned loading for decouping

data Action
  = Update
  | Setup
  | ListProf
  | InstallProf FilePath
  | BuildProf ID
  | RunProf ID
  | ChangeStart FilePath

optPrefs :: Opts.ParserPrefs
optPrefs = Opts.prefs Opts.showHelpOnEmpty

-- NOTE Completer doesn't work, maybe remove it?
manageOpts :: Opts.ParserInfo Action
manageOpts =
  (`Opts.info` Opts.fullDesc) . Opts.hsubparser $
    mconcat
      [ Opts.command "update" $
          Opts.info (pure Update) $
            Opts.progDesc "Updates xmonad-manage from source",
        Opts.command "setup" $
          Opts.info (pure Setup) $
            Opts.progDesc "Sets up common components, including XMonad",
        Opts.command "list" $
          Opts.info (pure ListProf) $
            Opts.progDesc "Lists installed profiles",
        Opts.command "install" $
          Opts.info (InstallProf <$> pathArg "<profile-path>") $
            Opts.progDesc "Installs a profile",
        Opts.command "build" $
          Opts.info (BuildProf <$> profIdArg) $
            Opts.progDesc "Builds a profile manually",
        Opts.command "run" $
          Opts.info (RunProf <$> profIdArg) $
            Opts.progDesc "Runs a profile",
        -- Startup is manually switched for now.
        Opts.command "startup" $
          Opts.info (ChangeStart <$> pathArg "<startup-path>") $
            Opts.progDesc "Change startup setups"
      ]
  where
    pathArg name = Opts.strArgument $ Opts.metavar name <> Opts.action "directory"
    profIdArg = Opts.argument (Opts.str >>= makeIDM) $ Opts.metavar "<profile-id>"

-- | The manager program. Current directory needs to be the profile main directory.
main :: IO ()
main = do
  home <- getHomeDirectory
  (`catch` handleError) $ do
    cmdLine <- unwords <$> getArgs
    saved <- get varMS
    let logger str = printf (printf "[%s] %s\n" cmdLine str)
        ManageSaved {managePath = envPath, profiles, startupDir} = saved
        mEnv = ManageEnv {envPath, logger}
        getProfile profID =
          maybe (throwIO $ ProfileNotFound $ Right profID) pure $ profiles M.!? profID

    Opts.customExecParser optPrefs manageOpts >>= \case
      -- Updates xmonad-manage
      Update -> do
        logger "Updating..."
        getExecutable "cabal" >>= (`callExe` ["install", "exe:xmonad-manage", "--overwrite-policy=always"])
        logger "Updated"

      -- Main installation
      Setup -> do
        logger "Begin"
        findExecutable "xmonad" >>= \case
          Just _ -> logger "xmonad found in PATH"
          Nothing -> callCommand "cabal install xmonad"
        logger "Install startup"
        getStartup startupDir >>= installStartup
        logger "End"

      -- Lists installed profiles
      ListProf -> do
        logger "Available profiles:"
        for_ (M.elems profiles) $ \cfgPath -> do
          Profile {profID, profProps = ProfileProps {..}} <- getProfileFromPath envPath cfgPath
          printf "- %s (%s)\n" (idStr profID) profileName
          printf "    %s\n" profileDetails

      -- Profile-specific installation
      InstallProf rawPath -> do
        cfgPath <- canonicalizePath rawPath
        logger "Begin"
        profile@Profile {profID} <- getProfileFromPath envPath cfgPath
        getExecutable "sudo" >>= installProfile mEnv profile
        let addProfile = M.insert profID cfgPath
        -- update saved
        varMS $~ \saved@ManageSaved {profiles} -> saved {profiles = addProfile profiles}
        logger "End"

      -- Manually build profile
      BuildProf profID -> do
        cfgPath <- getProfile profID
        logger "Begin"
        runXM <- runProfile mEnv <$> getProfileFromPath envPath cfgPath
        runXM True ["--recompile"]
        logger "End"

      -- Automatic profile run
      RunProf profID -> do
        cfgPath <- getProfile profID
        logger "Setup"
        runStart <- runStartup <$> getStartup startupDir
        withCurrentDirectory home runStart
        logger "Booting xmonad"
        runXM <- runProfile mEnv <$> getProfileFromPath envPath cfgPath
        withCurrentDirectory home $ runXM False []
        logger "Exit"

      -- Change startup
      ChangeStart rawPath -> do
        startupDir <- canonicalizePath rawPath
        logger "Begin"
        _ <- getStartup startupDir -- This checks if startup directory is valid
        varMS $~ \saved -> saved {startupDir}
        logger "Startup manage directory set to %s" startupDir
        logger "End"
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
