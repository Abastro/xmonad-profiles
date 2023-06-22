module Main (main) where

import Common
import Control.Exception
import Data.Foldable
import Data.Map.Strict qualified as M
import Data.StateVar
import Data.Text qualified as T
import Manages
import Options.Applicative qualified as Opts
import Packages
import Profile
import Startup
import System.Directory
import System.Environment
import System.Exit
import System.IO
import Text.Printf

-- NOTE Fetches from separate configuration directory for each profile.
-- TODO Maybe do not install system packages, and instead check for existence of packages?
-- Need analyzing dependencies - e.g. PulpMonad relies a lot on Gnome environment.

data Action
  = Update
  | ResetSave
  | Setup
  | ListProf
  | InstallProf FilePath
  | RemoveProf ID
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
            Opts.progDesc "Updates xmonad-manage from source."
      , Opts.command "reset-save" $
          Opts.info (pure ResetSave) $
            Opts.progDesc "Resets xmonad-manage save for when it is corrupted."
      , Opts.command "setup" $
          Opts.info (pure Setup) $
            Opts.progDesc "Sets up common components, including XMonad."
      , Opts.command "list" $
          Opts.info (pure ListProf) $
            Opts.progDesc "Lists installed profiles."
      , Opts.command "install" $
          Opts.info (InstallProf <$> pathArg "<profile-path>") $
            Opts.progDesc "Installs a profile."
      , Opts.command "remove" $
          Opts.info (RemoveProf <$> profIdArg) $
            Opts.progDesc "Removes a profile. Does not remove the cache files in '.cabal/store'."
      , Opts.command "build" $
          Opts.info (BuildProf <$> profIdArg) $
            Opts.progDesc "Builds a profile manually."
      , Opts.command "run" $
          Opts.info (RunProf <$> profIdArg) $
            Opts.progDesc "Runs a profile."
      , -- Startup is manually switched for now.
        Opts.command "startup" $
          Opts.info (ChangeStart <$> pathArg "<startup-path>") $
            Opts.progDesc "Change startup setups."
      ]
  where
    pathArg name = Opts.strArgument $ Opts.metavar name <> Opts.action "directory"
    profIdArg = Opts.argument (Opts.str >>= makeIDM) $ Opts.metavar "<profile-id>"

-- | The manager program. Current directory needs to be the profile main directory.
main :: IO ()
main = (`catch` handleError) $ do
  -- Sets to line buffering
  hSetBuffering stdout LineBuffering

  cmdLine <- unwords <$> getArgs
  saved <- get varMS
  let logger str = printf (printf "[%s] %s\n" cmdLine str)
      ManageSaved{managePath = envPath, profiles, startupDir} = saved
      mEnv = ManageEnv{envPath, logger}
      getProfile profID =
        maybe (throwIO $ ProfileNotFound profID) pure $ profiles M.!? profID

  Opts.customExecParser optPrefs manageOpts >>= \case
    -- Updates xmonad-manage
    Update -> do
      logger "Updating..."
      _ <- withCurrentDirectory envPath $ do
        cabal <- getExecutable "cabal"
        callExe cabal ["install", "exe:xmonad-manage", "--overwrite-policy=always"]
        get varMS -- In case it is updated, need to reset!
      logger "Updated"

    -- Resets the save if corrupted
    ResetSave -> do
      logger "*** Resetting save, data could be lost! ***"
      newMS <- mkMS
      varMS $= newMS
      logger "Save reset"

    -- Main installation
    Setup -> do
      logger "Begin"
      distro <- findDistro mEnv
      withDatabase mEnv $ \pkgDb -> do
        -- libxss is needed for xmonad, so this is hard-coded
        installPackages mEnv pkgDb distro [AsPackage $ T.pack "libxss"]
        findExecutable "xmonad" >>= \case
          Just _ -> logger "xmonad found in PATH"
          Nothing -> do
            cabal <- getExecutable "cabal"
            callExe cabal ["install", "xmonad"]
        logger "Install startup"
        startup <- getStartup startupDir
        installStartup mEnv pkgDb distro startup
      logger "End"

    -- Lists installed profiles
    ListProf -> do
      logger "Available profiles:"
      for_ (M.elems profiles) $ \cfgPath ->
        withProfile mEnv cfgPath $ \Profile{profID, profProps, cfgDir} -> do
          let ProfileProps{..} = profProps
          printf "- %s (%s)\n" (idStr profID) profileName
          printf "    Config at: %s\n" cfgDir
          printf "    %s\n" profileDetails

    -- Profile-specific installation
    InstallProf rawPath -> do
      cfgPath <- canonicalizePath rawPath
      logger "Begin"
      distro <- findDistro mEnv
      let onProfile prof@Profile{profID} = do
            withDatabase mEnv $ \pkgDb -> getExecutable "sudo" >>= installProfile mEnv pkgDb distro prof
            let addProfile = M.insert profID cfgPath
            varMS $~ \saved@ManageSaved{profiles} -> saved{profiles = addProfile profiles}
      withProfile mEnv cfgPath onProfile
      logger "End"

    -- Remove a profile
    RemoveProf profID -> do
      cfgPath <- getProfile profID
      logger "Begin"
      withProfile mEnv cfgPath $ \prof@Profile{profID} -> do
        getExecutable "sudo" >>= removeProfile mEnv prof
        let rmProfile = M.delete profID
        varMS $~ \saved@ManageSaved{profiles} -> saved{profiles = rmProfile profiles}
      logger "End"

    -- Manually build profile
    BuildProf profID -> do
      cfgPath <- getProfile profID
      logger "Begin"
      withProfile mEnv cfgPath (buildProfile mEnv)
      logger "End"

    -- Automatic profile run
    RunProf profID -> do
      home <- getHomeDirectory
      cfgPath <- getProfile profID
      withCurrentDirectory home $ do
        logger "Setup"
        getStartup startupDir >>= runStartup mEnv
        logger "Booting xmonad"
        withProfile mEnv cfgPath (runProfile mEnv)
        logger "Exit"

    -- Change startup
    ChangeStart rawPath -> do
      startupDir <- canonicalizePath rawPath
      logger "Begin"
      _ <- getStartup startupDir -- This checks if startup directory is valid
      varMS $~ \saved -> saved{startupDir}
      logger "Startup manage directory set to %s" startupDir
      logger "End"
  where
    -- MAYBE Do these need to be here?
    handleError = \case
      ProfileNotFound profID -> do
        hPrintf stderr "Error: Profile %s not found\n" (idStr profID)
        exitWith (ExitFailure 1)
      ProfileIOError profPath err -> do
        hPrintf stderr "Error: IO Exception while performing profile action on path %s\n" profPath
        hPrintf stderr "Details: %s\n" (show err)
        exitWith (ExitFailure 2)
      ProfileWrongFormat details -> do
        hPrintf stderr "Error: Profile cannot be read from profile.cfg\n"
        hPrintf stderr "Details: %s\n" details
        exitWith (ExitFailure 3)
