module Manages (
  SavedData (..),
  savedVar,
  restore,
  ManageEnv (..),
  withManageEnv,
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
  , configSharedDir, configUserDir :: !FilePath
  , moduleDir, databaseDir :: !FilePath
  , temporaryDir :: !FilePath
  }

withManageEnv :: (ManageEnv -> IO ()) -> IO ()
withManageEnv act = do
  home <- getHomeDirectory
  let configSharedDir = localDirectory FHSConfig </> "xmonad-manage"
      moduleDir = localDirectory FHSShare </> "xmonad-manage" </> "modules"
      databaseDir = localDirectory FHSShare </> "xmonad-manage" </> "database"
  configUserDir <- getXdgDirectory XdgConfig "xmonad-manage"
  withTemporaryDirectory $ \temporaryDir ->
    act ManageEnv{..}
