{-# LANGUAGE CPP #-}

module Main (main) where

import Common
import Component
import Control.Exception
import Data.Foldable
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.StateVar
import Data.Text qualified as T
import Data.Traversable
import Data.Tuple
import Manages
import Modules
import Options.Applicative qualified as Opts
import Packages
import Profile
import System.Directory
import System.Environment
import System.Exit
import System.IO
import System.Process
import Text.Printf

-- * Fetches from separate configuration directory for each profile.

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
  home <- getHomeDirectory
  ManageSaved{managePath = envPath, profiles} <- get varMS
  let logger str = printf (printf "[%s] %s\n" cmdLine str)
      mEnv = ManageEnv{envPath, logger}
      getProfile profID =
        maybe (throwIO $ ProfileNotFound profID) pure $ profiles M.!? profID
      (varModS, restoreModS) = mkVarModS mEnv

  Opts.customExecParser optPrefs manageOpts >>= \case
    -- Updates xmonad-manage
    Update -> do
      logger "Updating..."
      _ <- withCurrentDirectory envPath $ do
        callProcess "cabal" ["install", "exe:xmonad-manage", "--overwrite-policy=always"]
        -- In case it is updated, need to reset!
        get varMS
        get varModS
      logger "Updated."

    -- Resets the save if corrupted
    ResetSave -> do
      logger "*** Resetting save, data could be lost! ***"
      restoreMS
      restoreModS
      logger "Save reset."

    -- Main installation
    Setup installCond -> do
      logger "Begin"

      -- ? Improve UIs
      logger "Setting up modules..."
      ModuleSaved olds <- get varModS
      oldActives <- moduleInfos olds
      logger "Press enter to keep current active modules, or anything else to proceed with specification."
      getLine >>= \case
        [] -> logger "Proceeding with currently active modules..."
        _ -> do
          allBuiltIns <- moduleInfos (builtInModules mEnv)
          allTyped <- for [minBound .. maxBound] $ \typ -> do
            let builtIns = allBuiltIns M.! Just typ
                activeOnIt = listToMaybe $ oldActives M.! Just typ
            case activeOnIt of
              Just (_, name) -> logger "Active %s: %s." (show typ) name
              Nothing -> logger "No active module for %s." (show typ)
            logger "Available built-ins: %s" (show $ snd <$> builtIns)
            logger "Press enter to keep current active module, or specify as in following:"
            logger "1. To choose a built-in, type its name."
            logger "2. To use external module, type its path."
            logger "3. To avoid using a module for %s, type n." (show typ)
            getLine >>= \case
              [] -> do
                logger "Keep current active module for %s." (show typ)
                pure (fst <$> activeOnIt)
              name | Just path <- T.pack name `lookup` (swap <$> builtIns) -> do
                logger "Selected built-in %s for %s." name (show typ)
                pure (Just path)
              path -> do
                logger "Use external module located at %s" path
                pure (Just path)
          logger "Custom miscellaneous modules are not yet supported."
          varModS $= ModuleSaved (catMaybes allTyped)

      modules <- activeModules home =<< get varModS
      pkgDb <- getDatabase mEnv
      distro <- findDistro mEnv
      install mEnv pkgDb distro installCond (mconcat modules)
      logger "End"

    -- Lists installed profiles
    ListProf -> do
      logger "Available profiles:"
      for_ (M.elems profiles) $ \cfgPath -> do
        ProfileCfg{..} <- readProfileCfg cfgPath
        printf "- %s (%s)\n" (idStr profileID) profileProps.profileName
        printf "    Config at: %s\n" cfgPath
        printf "    %s\n" profileProps.profileDetails

    -- Profile-specific installation
    InstallProf rawPath installCond -> do
      cfgPath <- canonicalizePath rawPath
      logger "Begin"
      pkgDb <- getDatabase mEnv
      distro <- findDistro mEnv
      (profile, ident) <- loadProfile mEnv cfgPath
      install mEnv pkgDb distro installCond profile
      let addProfile = M.insert ident cfgPath
      varMS $~ \saved@ManageSaved{profiles} -> saved{profiles = addProfile profiles}
      logger "End"

    -- Remove a profile
    RemoveProf ident -> do
      cfgPath <- getProfile ident
      logger "Begin"
      -- Does not care about profile's own ID
      (profile, _) <- loadProfile mEnv cfgPath
      remove mEnv profile
      let rmProfile = M.delete ident
      varMS $~ \saved@ManageSaved{profiles} -> saved{profiles = rmProfile profiles}
      logger "End"

    -- Manually build profile
    BuildProf profID -> do
      cfgPath <- getProfile profID
      logger "Begin"
      (profile, _) <- loadProfile mEnv cfgPath
      invoke mEnv BuildMode profile
      logger "End"

    -- Automatic profile run
    RunProf profID -> do
      cfgPath <- getProfile profID
      withCurrentDirectory home $ do
        logger "Setup"
        modules <- activeModules home =<< get varModS
        invoke mEnv Start (mconcat modules)
        logger "Booting xmonad..."
        (profile, _) <- loadProfile mEnv cfgPath
        invoke mEnv RunMode profile
        logger "Exit"
  where
    -- MAYBE Do these need to be here?
    handleError = \case
      ProfileNotFound profID -> do
        hPrintf stderr "Error: Profile %s not found\n" (idStr profID)
        exitWith (ExitFailure 1)
      ProfileIOError profPath err -> do
        hPrintf stderr "Error: IO Exception while loading profile on path %s\n" profPath
        hPrintf stderr "Details: %s\n" (show err)
        exitWith (ExitFailure 2)
      ProfileWrongFormat details -> do
        hPrintf stderr "Error: Profile cannot be read from profile.cfg\n"
        hPrintf stderr "Details: %s\n" details
        exitWith (ExitFailure 3)
