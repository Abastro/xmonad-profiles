{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant <$>" #-}

module Main (main) where

import Common
import Component
import Control.Exception
import Data.Foldable
import Data.List
import Data.Map.Strict qualified as M
import Data.Proxy
import Data.StateVar
import Data.Text qualified as T
import GHC.IO.Handle
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
import Text.Printf
import X11

-- ? Consider: do not install system packages, and instead check for existence of packages?
-- ? Long-term: Add proxy for package removal.

-- ? Stay up-to-date appropriately corresponding to GHC Updates - How?
-- If this were statically compiled, it would not matter, but it will take more size.

--  * Profiles are kept for individual users,
--  * while data files and shared config files are shared in /usr/local.
--  * Also, each profile has separate configuration directory to fetch from.

-- ? Give choice for profile to keep separate directory for cabal installations. (CABAL_DIR)

-- TODO Later:
-- TODO IO Error handling & Printing everywhere
-- TODO 

data Action
  = ResetSave
  | Setup InstallCond
  | ListProf
  | InstallProf FilePath InstallCond
  | RemoveProf ID
  | BuildProf ID
  | RunProf ID
  | ViewStartLog

optPrefs :: Opts.ParserPrefs
optPrefs = Opts.prefs Opts.showHelpOnEmpty

manageOpts :: Opts.ParserInfo Action
manageOpts =
  (`Opts.info` Opts.fullDesc) . (Opts.simpleVersioner version <*>) . (Opts.helper <*>) . Opts.hsubparser $
    mconcat
      [ Opts.command "reset-save" $
          Opts.info (pure ResetSave) $
            Opts.progDesc "Resets xmonad-manage save for when it is corrupted."
      , Opts.command "setup" $
          Opts.info (Setup <$> installCond) $
            Opts.progDesc "Sets up common components, including XMonad."
      , Opts.command "list" $
          Opts.info (pure ListProf) $
            Opts.progDesc "Lists installed profiles."
      , Opts.command "install" $
          Opts.info (InstallProf <$> pathArg "<profile-id|profile-path>" <*> installCond) $
            Opts.progDesc "Installs or Re-installs a profile."
      , Opts.command "remove" $
          Opts.info (RemoveProf <$> profIdArg) $
            Opts.progDesc "Removes a profile. Does not remove the installed packages or cache files in '.cabal/store'."
      , Opts.command "build" $
          Opts.info (BuildProf <$> profIdArg) $
            Opts.progDesc "Builds a profile manually."
      , Opts.command "run" $
          Opts.info (RunProf <$> profIdArg) $
            Opts.progDesc "Runs a profile."
      , Opts.command "log-output" $
          Opts.info (pure ViewStartLog) $
            Opts.progDesc "View log output from running the xmonad-manage on login."
      ]
  where
    version = "xmonad-manage " <> VERSION_xmonad_manage
    pathArg name = Opts.strArgument $ Opts.metavar name <> Opts.action "directory"
    profIdArg = Opts.argument (Opts.str >>= makeIDM) $ Opts.metavar "<profile-id>"
    installCond =
      Opts.flag WhenAbsent AlwaysInstall $
        Opts.long "always-install"
          <> Opts.short 'a'
          <> Opts.help "Run package installation regardless of whether it was installed or not."

-- | The manager program. Current directory needs to be the profile main directory.
main :: IO ()
main = (`catch` handleError) $ do
  hSetBuffering stdout LineBuffering -- For consistent line buffering
  mEnv <- makeManageEnv
  Opts.customExecParser optPrefs manageOpts >>= handleOption mEnv
  where
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

handleOption :: ManageEnv -> Action -> IO ()
handleOption mEnv@ManageEnv{home} = \case
  -- Resets the save if corrupted
  ResetSave -> do
    putStrLn "*** Resetting save, data could be lost! ***"
    restore $ Proxy @InstalledProfiles
    putStrLn "Save reset."

  -- Main installation
  Setup installCond -> do
    actives <- activeModules mEnv

    putStrLn "Active modules to install:"
    printActive actives
    putStrLn "Proceed? (Ctrl+C to cancel)"
    _ <- getLine

    let combined = combineWithBuiltins x11Module actives
    pkgData <- loadPackageData mEnv
    install mEnv pkgData installCond combined

  -- Lists installed profiles
  ListProf -> do
    profiles <- profilePaths <$> get (savedVar @InstalledProfiles)
    putStrLn "Available profiles:"
    for_ profiles $ \cfgPath -> do
      ProfileSpec{..} <- readProfileSpec cfgPath
      printf "- %s (%s)\n" (idStr profileID) profileName
      printf "    Config at: %s\n" cfgPath
      printf "    %s\n" profileDetails

  -- Profile-specific installation
  InstallProf idOrPath installCond -> do
    ips <- get (savedVar @InstalledProfiles)
    cfgPath <- case (`getProfilePath` ips) =<< makeID idOrPath of
      Just cfgPath -> cfgPath <$ printf "Re-installing profile %s...\n" idOrPath
      Nothing -> do
        cfgPath <- canonicalizePath idOrPath
        cfgPath <$ printf "Installing profile from path %s...\n" cfgPath
    pkgData <- loadPackageData mEnv
    profile <- loadProfile cfgPath
    install mEnv pkgData installCond profile
    savedVar @InstalledProfiles $~ addProfile profile.identifier cfgPath

  -- Remove a profile
  RemoveProf profID -> withProfPath profID $ \cfgPath -> do
    -- Profile is marked removed first to avoid considering broken installs as valid
    savedVar @InstalledProfiles $~ removeProfile profID
    remove mEnv =<< loadProfile cfgPath

  -- Manually build profile
  BuildProf profID -> withProfPath profID $ \cfgPath -> do
    invoke mEnv BuildMode =<< loadProfile cfgPath

  -- Automatic profile run
  RunProf profID -> withProfPath profID $ \cfgPath -> do
    redirectLogs
    putStrLn "Inherited environment:"
    getEnvironment >>= traverse_ (uncurry $ printf "%s=%s\n")
    putStrLn ""
    sendEnvToService
    updatePATH home -- PATH needs updating
    withCurrentDirectory home $ do
      actives <- activeModules mEnv
      invoke mEnv Start (combineWithBuiltins x11Module actives)

      putStrLn "Booting xmonad..."
      invoke mEnv RunMode =<< loadProfile cfgPath
  ViewStartLog -> do
    logDir <- getXdgDirectory XdgState "xmonad-manage"
    putStrLn "Output:"
    putStrLn =<< readFile (logDir </> "start.out")
    putStrLn ""
    putStrLn "Error:"
    putStrLn =<< readFile (logDir </> "start.err")
  where
    withProfPath profID act = do
      ips <- get (savedVar @InstalledProfiles)
      case getProfilePath profID ips of
        Nothing -> throwIO (ProfileNotFound profID)
        Just profilePath -> act profilePath

    printActive :: ModuleSet (Component ModuleMode) -> IO ()
    printActive modules = do
      for_ [minBound .. maxBound] $ \typ -> do
        case modules.typedModules M.!? typ of
          Just mod -> printf "%s: %s\n" (show typ) (show mod.identifier)
          Nothing -> printf "%s: None\n" (show typ)
      printf "Others: %s\n" $ show $ (\mod -> mod.identifier) <$> modules.otherModules

    -- Send current environment to the service file.
    sendEnvToService :: IO ()
    sendEnvToService = do
      putStrLn "Sending environments to systemd..."
      getEnvironment >>= traverse_ (\(k, v) -> setServiceEnv k (T.pack v))

    updatePATH :: FilePath -> IO ()
    updatePATH home = do
      path <- getEnv "PATH"
      -- Blame ghcup for not putting environment inside .profile, duh
      let newPath =
            intercalate
              ":"
              [ home </> ".cabal" </> "bin"
              , home </> ".ghcup" </> "bin"
              , path
              ]
      printf "Updating path to \"%s\"...\n" newPath
      setEnv "PATH" newPath
      setServiceEnv "PATH" (T.pack newPath)

    redirectLogs :: IO ()
    redirectLogs = do
      logDir <- getXdgDirectory XdgState "xmonad-manage"
      -- Redirect stdout/stderr elsewhere; lightdm is being goofy about it..
      -- You need to look into ~/.xsession-errors if something happens beforehand.
      createDirectoryIfMissing True logDir
      handleOut <- openFile (logDir </> "start.out") WriteMode
      handleErr <- openFile (logDir </> "start.err") WriteMode

      hDuplicateTo handleOut stdout
      hDuplicateTo handleErr stderr
      hClose handleOut
      hClose handleErr

      hSetBuffering stdout LineBuffering
      hSetBuffering stderr LineBuffering
      putStrLn "> Log redirected. <"
