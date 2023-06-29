{-# LANGUAGE CPP #-}

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
import System.Process

-- * Fetches from separate configuration directory for each profile.

-- ? Install & Run in terms of modules? Think about this.
-- TODO Consider how systemd services are run
-- ? Long-term: Add proxy for package removal.

-- ? Consider: do not install system packages, and instead check for existence of packages?
-- ? Stay up-to-date appropriately corresponding to GHC Updates - How?
-- If this were statically compiled, it would not matter, but it will take more size.
-- ? Need analyzing dependencies - e.g. PulpMonad relies a lot on Gnome environment.

data Action
  = Update
  | ResetSave
  | Setup InstallCond
  | ListProf
  | InstallProf FilePath InstallCond
  | RemoveProf ID
  | BuildProf ID
  | RunProf ID
  | ChangeStart FilePath

optPrefs :: Opts.ParserPrefs
optPrefs = Opts.prefs Opts.showHelpOnEmpty

manageOpts :: Opts.ParserInfo Action
manageOpts =
  (`Opts.info` Opts.fullDesc) . (Opts.simpleVersioner version <*>) . (Opts.helper <*>) . Opts.hsubparser $
    mconcat
      [ Opts.command "update" $
          Opts.info (pure Update) $
            Opts.progDesc "Updates xmonad-manage from source."
      , Opts.command "reset-save" $
          Opts.info (pure ResetSave) $
            Opts.progDesc "Resets xmonad-manage save for when it is corrupted."
      , Opts.command "setup" $
          Opts.info (Setup <$> installCond) $
            Opts.progDesc "Sets up common components, including XMonad."
      , Opts.command "list" $
          Opts.info (pure ListProf) $
            Opts.progDesc "Lists installed profiles."
      , Opts.command "install" $
          Opts.info (InstallProf <$> pathArg "<profile-path>" <*> installCond) $
            Opts.progDesc "Installs a profile."
      , Opts.command "remove" $
          Opts.info (RemoveProf <$> profIdArg) $
            Opts.progDesc "Removes a profile. Does not remove the installed packages or cache files in '.cabal/store'."
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
    version = "xmonad-manage " <> VERSION_xmonad_manage
    pathArg name = Opts.strArgument $ Opts.metavar name <> Opts.action "directory"
    profIdArg = Opts.argument (Opts.str >>= makeIDM) $ Opts.metavar "<profile-id>"
    installCond =
      Opts.flag
        WhenAbsent
        AlwaysInstall
        ( Opts.long "always-install"
            <> Opts.short 'a'
            <> Opts.help "Run installation regardless of whether it was installed or not."
        )

-- | The manager program. Current directory needs to be the profile main directory.
main :: IO ()
main = (`catch` handleError) $ do
  -- For consistent line buffering
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
        callProcess "cabal" ["install", "exe:xmonad-manage", "--overwrite-policy=always"]
        get varMS -- In case it is updated, need to reset!
      logger "Updated"

    -- Resets the save if corrupted
    ResetSave -> do
      logger "*** Resetting save, data could be lost! ***"
      newMS <- mkMS
      varMS $= newMS
      logger "Save reset"

    -- Main installation
    Setup installCond -> do
      logger "Begin"
      distro <- findDistro mEnv
      -- XMonad's requirement, libxss is a source dependency of xmonad.
      let xmonadReq = requireDeps [AsPackage (T.pack "libxss"), AsPackage (T.pack "xmonad")]
      withDatabase mEnv $ \pkgDb -> do
        withStartup startupDir $ \startup -> do
          meetRequirements mEnv pkgDb distro installCond (xmonadReq <> x11Reqs <> startupReqs startup)
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
    InstallProf rawPath installCond -> do
      cfgPath <- canonicalizePath rawPath
      logger "Begin"
      distro <- findDistro mEnv
      withDatabase mEnv $ \pkgDb -> do
        withProfile mEnv cfgPath $ \profile@Profile{profID} -> do
          meetRequirements mEnv pkgDb distro installCond (profileReqs profile)
          let addProfile = M.insert profID cfgPath
          varMS $~ \saved@ManageSaved{profiles} -> saved{profiles = addProfile profiles}
      logger "End"

    -- Remove a profile
    RemoveProf profID -> do
      cfgPath <- getProfile profID
      logger "Begin"
      withProfile mEnv cfgPath $ \profile@Profile{profID} -> do
        stopRequirements mEnv (profileReqs profile)
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
        withStartup startupDir (runStartup mEnv)
        logger "Booting xmonad"
        withProfile mEnv cfgPath (runProfile mEnv)
        logger "Exit"

    -- Change startup
    ChangeStart rawPath -> do
      startupDir <- canonicalizePath rawPath
      logger "Begin"
      withStartup startupDir $ \_ -> pure () -- This checks if startup directory is valid
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
