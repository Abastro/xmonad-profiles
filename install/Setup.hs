#!/usr/bin/env cabal
{- cabal:
build-depends:
  base,
  filepath,
  directory,
  process
-}
-- Requires: gnome desktop, ghcup, cabal
{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Control.Exception
import Control.Monad
import Data.Foldable
import System.Environment
import System.Exit
import System.FilePath
import System.Process
import Text.Printf
import System.Directory

makeExecutable :: [FilePath] -> IO ()
makeExecutable paths =
  lines <$> readCreateProcess (shell "command -v chmod") "" >>= \case
    [] -> ioError (userError "System: chmod does not exist")
    (chmod : _) -> traverse_ (\file -> callProcess chmod ["+x", file]) paths

whenNotExist :: String -> IO () -> IO ()
whenNotExist cmd act =
  readCreateProcessWithExitCode (shell $ unwords ["command", "-v", cmd]) "" >>= \case
    (ExitFailure _, _, _) -> act
    _ -> pure ()

runAsSudo :: [String] -> IO ()
runAsSudo args =
  lines <$> readCreateProcess (shell "command -v sudo") "" >>= \case
    [] -> ioError (userError "System: sudo does not exist")
    (sudo : _) -> callProcess sudo ("-S" : args)

installMain :: FilePath -> IO ()
installMain installDir = do
  putStrLn "[Main] Install Begin"

  makeExecutable $
    map (installDir </>) ["xmonad.start", "xmonad.setup", "xmonad.select"]

  whenNotExist "xmonad" $
    callCommand "cabal install xmonad"

  runAsSudo $
    ["apt", "install"]
      <> ["xcompmgr", "suckless-tools", "gnome-screensaver", "xss-lock", "gnome-keyring"]

  putStrLn "[Main] Install End"

installProfile :: FilePath -> String -> IO ()
installProfile installDir profile = do
  printf "[%s] Install Begin\n" profile

  let cacheDir = installDir </> "cache"
  let dataDir = installDir </> "data"
  let profileDir = installDir </> profile

  makeExecutable $
    map (profileDir </>) ["install", "build"]

  -- Assumes that /usr/share/xsessions exist
  writeFile (cacheDir </> profile <.> "desktop") runner
  runAsSudo ["mv", cacheDir </> profile <.> "desktop", "/usr/share/xsessions"]

  callProcess (profileDir </> "install") []

  -- Finally, try recompile using xmonad
  setEnv "XMONAD_DATA_DIR" (dataDir </> profile)
  setEnv "XMONAD_CONFIG_DIR" profileDir
  setEnv "XMONAD_CACHE_DIR" (cacheDir </> profile)
  callCommand "xmonad --recompile"

  printf "[%s] Install End\n" profile
  where
    runner =
      unlines
        [ printf "[Desktop Entry]",
          printf "Encoding=UTF-8",
          printf "Name=%s" profile,
          printf "Comment=Xmonad profile <%s>" profile,
          printf "Exec=%s/xmonad.start %s" installDir profile,
          printf "Type=XSession"
        ]

main :: IO ()
main =
  putStrLn "" >> getArgs >>= \case
    [] -> putStrLn "Error: requires install directory parameter"
    dir : installs -> do
      installDir <- makeAbsolute dir
      installMain installDir
      traverse_ (installProfile installDir) installs
