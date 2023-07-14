module Manages (
  ManageSaved (..),
  ManageEnv (..),
  varMS,
  restoreMS,
  loadConfig,
)
where

import Common
import Control.Exception
import Data.Map.Strict qualified as M
import Data.Serialize (Serialize)
import Data.StateVar
import GHC.Generics (Generic)
import System.Directory
import System.FilePath
import System.IO
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
  , home :: !FilePath
  -- ^ Home directory for easier referencing
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

-- | Loads configuration from /config folder, and copies the template if it fails.
loadConfig :: ManageEnv -> String -> (FilePath -> IO a) -> IO a
loadConfig ManageEnv{..} cfgName reader =
  catch (reader configPath) $ \(exc :: IOException) -> do
    logger "Cannot identify the configuration %s due to:" cfgName
    hPrint stderr exc
    logger "Reverting configuration to default."
    createDirectoryIfMissing True (envPath </> "config")
    copyFile templatePath configPath
    logger "Trying again..."
    reader configPath
  where
    templatePath = envPath </> "database" </> cfgName
    configPath = envPath </> "config" </> cfgName
