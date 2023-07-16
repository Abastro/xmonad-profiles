module Manages (
  ManageSaved (..),
  ManageEnv (..),
  varMS,
  restoreMS,
  loadConfig,
)
where

import Common
import Control.Applicative
import Control.Exception
import Control.Monad
import Data.ByteString.Lazy qualified as B
import Data.Map.Strict qualified as M
import Data.Serialize
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
  }

mkMS :: IO ManageSaved
mkMS = do
  putStrLn "Manager path not yet specified, setting to current directory"
  managePath <- getCurrentDirectory
  pure $ ManageSaved{managePath, profiles = M.empty, startupDir = managePath </> "start-lightdm"}

varMS :: StateVar ManageSaved
restoreMS :: IO ()
(varMS, restoreMS) = dataVar "xmonad-manage" "manage-data" mkMS

-- | Data variable stored in XDG_DATA_DIR, with an action to reset it to default.
dataVar :: (Serialize a) => String -> String -> IO a -> (StateVar a, IO ())
dataVar appName varName mkDef = (var, restore)
  where
    var = makeStateVar load save
    restore = mkDef >>= (var $=)
    load = do
      msave <- datPath
      let readAsA = withFile msave ReadMode (evaluate <=< fmap decodeLazy . B.hGetContents)
      readAsA <|> pure (Left "") >>= \case
        Right saved -> pure saved
        Left _ -> do
          defVal <- mkDef
          defVal <$ B.writeFile msave (encodeLazy defVal)
    save saved = do
      msave <- datPath
      B.writeFile msave (encodeLazy saved)

    datPath = do
      dataDir <- getXdgDirectory XdgData appName
      createDirectoryIfMissing True dataDir
      (dataDir </> varName) <$ setPermissions dataDir perm
    perm = setOwnerSearchable True . setOwnerReadable True . setOwnerWritable True $ emptyPermissions

-- | Loads configuration from /config folder, and copies the template if it fails.
loadConfig :: ManageEnv -> String -> (FilePath -> IO a) -> IO a
loadConfig ManageEnv{..} cfgName reader =
  catch (reader configPath) $ \(exc :: IOException) -> do
    printf "Cannot identify the configuration %s due to:\n" cfgName
    hPrint stderr exc
    printf "Reverting configuration to default.\n"
    createDirectoryIfMissing True (envPath </> "config")
    copyFile templatePath configPath
    printf "Trying again...\n"
    reader configPath
  where
    templatePath = envPath </> "database" </> cfgName
    configPath = envPath </> "config" </> cfgName
