module Manages (
  SavedData (..),
  savedVar,
  restore,
  ManageEnv (..),
  makeManageEnv
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
import System.Directory
import System.FilePath
import System.IO

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

data ManageEnv = ManageEnv
  { home :: !FilePath
  , configDir :: !FilePath
  , databaseDir :: !FilePath
  }

makeManageEnv :: IO ManageEnv
makeManageEnv = do
  home <- getHomeDirectory
  let configDir = localDirectory FHSConfig </> "xmonad-manage"
      databaseDir = localDirectory FHSShare </> "xmonad-manage"
  pure ManageEnv{..}

{-
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
-}