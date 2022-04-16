#!/usr/bin/env cabal
{- cabal:
build-depends:
  base,
  filepath ==1.4.2.2
-}
module Main ( main ) where

-- If this were to make sense, it should be a proper project
main :: IO ()
main = putStrLn "Hello, world!"
