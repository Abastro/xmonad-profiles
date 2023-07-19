module Manages (
  SavedData (..),
  savedVar,
  restore,
  ManageSaved (..),
  ManageEnv (..),
  loadConfig,
)
where

import Common
import Control.Applicative
import Control.Exception
import Control.Monad
import Data.ByteString.Lazy qualified as B
import Data.Proxy
import Data.Serialize
import Data.StateVar
import GHC.Generics (Generic)
import System.Directory
import System.FilePath
import System.IO
import Text.Printf

class (Serialize a) => SavedData a where
  dataName :: Proxy a -> FilePath
  initialize :: IO a

savedVar :: forall a. (SavedData a) => StateVar a
savedVar = makeStateVar load save
  where
    load = do
      savePath <- datPath
      let readAsA = withFile savePath ReadMode (evaluate <=< fmap decodeLazy . B.hGetContents)
      readAsA <|> pure (Left "error") >>= \case
        Right saved -> pure saved
        Left _ -> do
          defVal <- initialize
          defVal <$ B.writeFile savePath (encodeLazy defVal)
    save dat = do
      savePath <- datPath
      B.writeFile savePath (encodeLazy dat)

    datPath = do
      dataDir <- getXdgDirectory XdgData "xmonad-manage"
      createDirectoryIfMissing True dataDir
      pure $ dataDir </> dataName (Proxy @a)

restore :: forall a. (SavedData a) => Proxy a -> IO ()
restore Proxy = initialize @a >>= (savedVar $=)

newtype ManageSaved = ManageSaved {managePath :: FilePath}
  deriving (Show, Generic)

instance Serialize ManageSaved
instance SavedData ManageSaved where
  dataName :: Proxy ManageSaved -> FilePath
  dataName Proxy = "manage-data"
  initialize :: IO ManageSaved
  initialize = do
    putStrLn "Manager path not yet specified, setting to current directory"
    managePath <- getCurrentDirectory
    pure $ ManageSaved{managePath}

data ManageEnv = ManageEnv
  { envPath :: !FilePath
  , home :: !FilePath
  -- ^ Home directory for easier referencing
  }

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
