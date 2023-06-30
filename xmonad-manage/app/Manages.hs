module Manages (
  ManageSaved (..),
  ManageEnv (..),
  varMS,
  restoreMS,
)
where

import Common
import Data.Map.Strict qualified as M
import Data.Serialize (Serialize)
import Data.StateVar
import GHC.Generics (Generic)
import System.Directory
import System.FilePath
import Text.Printf

data ManageSaved = ManageSaved
  { managePath :: !FilePath
  , profiles :: !(M.Map ID FilePath)
  , startupDir :: !FilePath
  }
  deriving (Show, Generic)

instance Serialize ManageSaved

data ManageEnv = ManageEnv
  { envPath :: !FilePath
  , logger :: forall r. (PrintfType r) => String -> r
  }

mkMS :: IO ManageSaved
mkMS = do
  putStrLn "Manager path not yet specified, setting to current directory"
  managePath <- getCurrentDirectory
  pure $ ManageSaved{managePath, profiles = M.empty, startupDir = managePath </> "start-lightdm"}

varMS :: StateVar ManageSaved
restoreMS :: IO ()
(varMS, restoreMS) = dataVar "xmonad-manage" "manage-data" mkMS
