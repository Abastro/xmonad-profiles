{-# LANGUAGE CPP #-}

module Main (main) where

import Common
import Component
import Control.Exception
import Data.Foldable
import Data.List
import Data.Map.Strict qualified as M
import Data.StateVar
import Manages
import Modules
import Options.Applicative qualified as Opts
import Packages
import Profile
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.Process
import Text.Printf
import X11

-- * Fetches from separate configuration directory for each profile.

-- ? Long-term: Add proxy for package removal.

-- ? xmonad-manage might need to be installed in global directory.
-- ? Or, "do not go through xmonad-manage" - simpler way. Think about this.

-- ? Consider: do not install system packages, and instead check for existence of packages?
-- ? Stay up-to-date appropriately corresponding to GHC Updates - How?
-- If this were statically compiled, it would not matter, but it will take more size.
-- ? Need analyzing dependencies - e.g. PulpMonad relies a lot on Gnome environment.

-- TODO Further adopt XDG Directory

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
  hSetBuffering stdout LineBuffering -- For consistent line buffering
  cmdLine <- unwords <$> getArgs
  home <- getHomeDirectory

  ManageSaved{managePath = envPath, profiles} <- get varMS
  let logger str = printf (printf "[%s] %s\n" cmdLine str)
      mEnv = ManageEnv{..}
  Opts.customExecParser optPrefs manageOpts >>= handleOption mEnv profiles
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

handleOption :: ManageEnv -> M.Map ID FilePath -> Action -> IO ()
handleOption mEnv@ManageEnv{..} profiles = \case
  Update -> region "Updating..." "Updated." $ withCurrentDirectory envPath $ do
    withTemporaryDirectory $ \tmpDir -> do
      -- To install into /opt/bin, build into temporary directory and then move it.
      callProcess
        "cabal"
        [ "install"
        , "exe:xmonad-manage"
        , "--overwrite-policy=always"
        , "--disable-executable-dynamic"
        , "--install-method=copy"
        , "--install-dir=" <> tmpDir
        ]
      callProcess "sudo" ["mkdir", "-p", thisInstallDirectory]
      callProcess "sudo" ["cp", tmpDir </> "xmonad-manage", thisInstallDirectory]
    -- In case it is updated, need to reset!
    get varMS

  -- Resets the save if corrupted
  ResetSave -> do
    logger "*** Resetting save, data could be lost! ***"
    restoreMS
    logger "Save reset."

  -- Main installation
  Setup installCond -> beginEnd $ do
    activeCfg <- loadActiveCfg mEnv

    logger "Active modules to install:"
    activeModuleData mEnv activeCfg >>= printActive
    logger "Proceed? (Ctrl+C to cancel)"
    _ <- getLine

    modules <- activeModules mEnv loadX11Module activeCfg
    pkgDb <- getDatabase mEnv
    distro <- findDistro mEnv
    install mEnv pkgDb distro installCond (mconcat modules)

  -- Lists installed profiles
  ListProf -> do
    logger "Available profiles:"
    for_ (M.elems profiles) $ \cfgPath -> do
      ProfileSpec{..} <- readProfileSpec cfgPath
      printf "- %s (%s)\n" (idStr profileID) profileProps.profileName
      printf "    Config at: %s\n" cfgPath
      printf "    %s\n" profileProps.profileDetails

  -- Profile-specific installation
  InstallProf rawPath installCond -> beginEnd $ do
    cfgPath <- canonicalizePath rawPath
    pkgDb <- getDatabase mEnv
    distro <- findDistro mEnv
    (profile, ident) <- loadProfile mEnv cfgPath
    install mEnv pkgDb distro installCond profile
    let addProfile = M.insert ident cfgPath
    varMS $~ \saved@ManageSaved{profiles} -> saved{profiles = addProfile profiles}

  -- Remove a profile
  RemoveProf ident -> beginEnd $ do
    cfgPath <- getProfile ident
    -- Does not care about profile's own ID
    (profile, _) <- loadProfile mEnv cfgPath
    remove mEnv profile
    let rmProfile = M.delete ident
    varMS $~ \saved@ManageSaved{profiles} -> saved{profiles = rmProfile profiles}

  -- Manually build profile
  BuildProf profID -> beginEnd $ withProfPath profID $ \cfgPath -> do
    (profile, _) <- loadProfile mEnv cfgPath
    invoke mEnv BuildMode profile

  -- Automatic profile run
  RunProf profID -> region "Setup" "Exit" $ withProfPath profID $ \cfgPath -> do
    withCurrentDirectory home $ do
      -- PATH needs updating
      updatePATH home
      modules <- activeModules mEnv loadX11Module =<< loadActiveCfg mEnv
      invoke mEnv Start (mconcat modules)
      logger "Booting xmonad..."
      (profile, _) <- loadProfile mEnv cfgPath
      invoke mEnv RunMode profile
  where
    getProfile profID =
      maybe (throwIO $ ProfileNotFound profID) pure $ profiles M.!? profID

    region :: String -> String -> IO a -> IO ()
    region enter exit act = logger enter *> act *> logger exit
    beginEnd = region "Begin" "End"

    withProfPath profID act = case profiles M.!? profID of
      Nothing -> throwIO (ProfileNotFound profID)
      Just profilePath -> act profilePath

    printActive :: ModuleSet ModuleSpec -> IO ()
    printActive modules = do
      for_ [minBound .. maxBound] $ \typ -> do
        case modules.typedModules M.!? typ of
          Just mod -> logger "%s: %s" (show typ) mod.name
          Nothing -> logger "%s: None" (show typ)
      logger "Others: %s" $ show $ (\mod -> mod.name) <$> modules.otherModules

    updatePATH :: FilePath -> IO ()
    updatePATH home = do
      path <- getEnv "PATH"
      -- Blame ghcup for not putting environment inside .profile, duh
      -- ? Maybe check if these are added in PATH beforehand
      let newPath =
            intercalate
              ":"
              [ home </> ".cabal" </> "bin"
              , home </> ".ghcup" </> "bin"
              , thisInstallDirectory
              , path
              ]
      setEnv "PATH" newPath
      setServiceEnv "PATH" newPath
