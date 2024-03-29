{-# LANGUAGE DerivingStrategies #-}

-- | Checked stuffs.
module Common (
  module Prelude,
  module Control.Category,
  FHSDirectory (..),
  localDirectory,
  setToExecutable,
  withTemporaryDirectory,
  ID (UnsafeMakeID),
  idStr,
  makeID,
  makeIDM,
  readYAMLFile,
  setNormalEnv,
  setServiceEnv,
  ServiceDirectory (..),
  getServiceDirectory,
  ShellString,
  parseShellString,
  readShellStringFile,
  shellExpandWith,
  shellExpand,
  shellExpandFromMap,
) where

import Control.Category
import Control.Exception
import Control.Monad
import Data.ByteString.Lazy qualified as B
import Data.Char
import Data.Coerce
import Data.Map.Strict qualified as M
import Data.Serialize
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.YAML
import System.Directory
import System.Environment
import System.FilePath
import System.Posix hiding (getEnv, setEnv)
import System.Process
import Text.Parsec qualified as P
import Text.Printf
import Prelude hiding (id, (.))

data FHSDirectory = FHSBin | FHSLib | FHSConfig | FHSShare
fhsPath :: FHSDirectory -> FilePath
fhsPath = \case
  FHSBin -> "bin"
  FHSLib -> "lib"
  FHSConfig -> "etc"
  FHSShare -> "share"

-- | This directory is visible to all users; it is "local" in the sense that software is locally installed.
localDirectory :: FHSDirectory -> FilePath
localDirectory fhsDir = "/usr/local" </> fhsPath fhsDir

setToExecutable :: FilePath -> IO ()
setToExecutable path = do
  -- Generic one is not reliable, setting executable permission for all users.
  mode <- fileMode <$> getFileStatus path
  let newMode = foldr unionFileModes mode [ownerExecuteMode, groupExecuteMode, otherExecuteMode]
  setFileMode path newMode

withTemporaryDirectory :: (FilePath -> IO ()) -> IO ()
withTemporaryDirectory = bracket makeTempDir removePathForcibly
  where
    makeTempDir = do
      name <- getProgName
      tempDir <- unwords . words <$> readProcess "mktemp" ["-d", "/tmp" </> name <.> "XXXXXXXXXXXX"] []
      createDirectoryIfMissing True tempDir
      pure tempDir

-- | Denotes ID, made of ASCII letters w/o space
newtype ID = UnsafeMakeID String deriving newtype (Show, Eq, Ord, Serialize)

idStr :: ID -> String
idStr = coerce

makeID :: String -> Maybe ID
makeID ident = UnsafeMakeID ident <$ guard (all isAscii ident && not (any isSpace ident))

makeIDM :: (MonadFail f) => String -> f ID
makeIDM ident = maybe (fail failMsg) pure $ makeID ident
  where
    failMsg = printf "ID %s contains illegal letter or spaces" ident

instance FromYAML ID where
  parseYAML :: Node Pos -> Parser ID
  parseYAML n = parseYAML n >>= makeIDM . T.unpack

readYAMLFile :: (FromYAML a, Exception e) => (String -> e) -> FilePath -> IO a
readYAMLFile formatErr path = do
  file <- B.readFile path
  case decode1 file of
    Left (pos, err) -> (throwIO . formatErr) (prettyPosWithSource pos file "Wrong format" <> err)
    Right st -> pure st

-- | setEnv, but with Text.
setNormalEnv :: String -> T.Text -> IO ()
setNormalEnv name val = setEnv name (T.unpack val)

-- | Sets environment for both dbus and systemd services.
setServiceEnv :: String -> T.Text -> IO ()
setServiceEnv name val = do
  callProcess
    "dbus-update-activation-environment"
    [ "--systemd"
    , name <> "=" <> T.unpack val
    ]

data ServiceDirectory = GlobalService | PerUserService

getServiceDirectory :: ServiceDirectory -> IO FilePath
getServiceDirectory = \case
  GlobalService -> pure ("/etc" </> "systemd" </> "user")
  PerUserService -> (</> "user") <$> getXdgDirectory XdgConfig "systemd"

-- | Denotes a string with shell variables embedded.
newtype ShellString = MkShellStr [ShellStrElem]
  deriving (Show)

data ShellStrElem = Str !T.Text | Var !T.Text
  deriving (Show)

-- | Example:
--
-- >>> parseShellString "example text" "Hello, ${NAME}! ${GREETINGS}."
-- MkShellStr [Str "Hello, ",Var "NAME",Str "! ",Var "GREETINGS",Str "."]
--
-- >>> parseShellString "error example" "Unmatched ${ bracket"
-- user error ("error example" (line 1, column 21):
-- unexpected end of input
-- expecting variable)
parseShellString :: (MonadFail m) => String -> T.Text -> m ShellString
parseShellString name txt = case P.parse shellStr name txt of
  Left err -> fail (show err)
  Right res -> pure res
  where
    shellStr = MkShellStr <$> P.many elem <* P.eof
    elem = P.try (Var <$> shellVar P.<|> Str <$> string)
    -- I do not want to pull megaparsec, and this is the most I could do with parsec
    string = P.try (T.pack <$> P.many1 (P.noneOf ['$'])) P.<?> "string"
    shellVar =
      P.try (T.pack <$> P.between opens closes (P.many1 $ P.noneOf ['}'])) P.<?> "variable"
    opens = P.try (P.string "${")
    closes = P.try (P.string "}")

instance FromYAML ShellString where
  parseYAML :: Node Pos -> Parser ShellString
  parseYAML = withStr "shell-string" (parseShellString "shell")

readShellStringFile :: FilePath -> IO ShellString
readShellStringFile path = T.readFile path >>= parseShellString path

shellExpandWith :: (MonadFail m) => (T.Text -> m T.Text) -> ShellString -> m T.Text
shellExpandWith act (MkShellStr strs) = mconcat <$> traverse expand strs
  where
    expand = \case
      Str str -> pure str
      Var var -> act var

shellExpand :: ShellString -> IO T.Text
shellExpand = shellExpandWith (fmap T.pack . getEnv . T.unpack)

shellExpandFromMap :: (MonadFail m) => (forall a. String -> m a) -> M.Map String T.Text -> ShellString -> m T.Text
shellExpandFromMap onMissing refMap = shellExpandWith $ \key -> case refMap M.!? T.unpack key of
  Just val -> pure val
  Nothing -> onMissing (T.unpack key)
