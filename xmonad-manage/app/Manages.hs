module Manages
  ( ManageSaved (..),
    ManageEnv (..),
    varMS,
  )
where

import Common
import Data.Map.Strict qualified as M
import Data.StateVar
import System.Directory
import System.FilePath
import Text.Printf
import GHC.Generics (Generic)
import Data.Serialize (Serialize)

data ManageSaved = ManageSaved
  { managePath :: !FilePath,
    profiles :: !(M.Map ID FilePath),
    startupDir :: !FilePath
  }
  deriving (Show, Generic)

instance Serialize ManageSaved

data ManageEnv = ManageEnv
  { envPath :: !FilePath,
    logger :: forall r. PrintfType r => String -> r
  }

varMS :: StateVar ManageSaved
varMS = dataVar "xmonad-manage" "manage-data" $ do
  putStrLn "Manager path not yet specified, setting to current directory"
  managePath <- getCurrentDirectory
  pure $ ManageSaved {managePath, profiles = M.empty, startupDir = managePath </> "start-basic"}
