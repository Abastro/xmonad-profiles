{-# LANGUAGE DerivingStrategies #-}

-- | Checked stuffs.
module Common (
  setToExecutable,
  dataVar,
  ID,
  idStr,
  makeID,
  makeIDM,
  readYAMLFile,
  setEnv,
  setServiceEnv,
  ServiceType (..),
  ServiceStream (..),
  Service (..),
  serviceFile,
  ShellString,
  parseShellString,
  shellExpandWith,
  shellExpand,
) where

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.ByteString.Lazy qualified as B
import Data.Char
import Data.Coerce
import Data.Serialize
import Data.StateVar
import Data.Text qualified as T
import Data.YAML
import System.Directory
import System.Environment
import System.FilePath
import System.IO
import System.Process
import Text.Parsec qualified as P
import Text.Printf

setToExecutable :: FilePath -> IO ()
setToExecutable path = do
  perm <- getPermissions path
  setPermissions path (setOwnerExecutable True perm)

-- | Data variable stored in XDG_DATA_DIR, with an action to reset it to default.
dataVar :: (Serialize a) => String -> String -> IO a -> (StateVar a, IO ())
dataVar appName loc mkDef = (var, restore)
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
      (dataDir </> loc) <$ setPermissions dataDir perm
    perm = setOwnerSearchable True . setOwnerReadable True . setOwnerWritable True $ emptyPermissions

-- | Denotes ID, made of ASCII letters w/o space
newtype ID = ID String deriving newtype (Show, Eq, Ord, Serialize)

idStr :: ID -> String
idStr = coerce

makeID :: String -> Maybe ID
makeID ident = ID ident <$ guard (all isAscii ident && not (any isSpace ident))

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

-- | Sets environment for services.
setServiceEnv :: String -> String -> IO ()
setServiceEnv name val = do
  callProcess "systemctl" ["--user", "set-environment", name <> "=" <> val]

-- Well, I set this up, but seems not so useful
data ServiceType = Simple | Exec
data ServiceStream = Journal | FileWrite !FilePath

-- | A service definition for systemd application.
data Service = MkService
  { serviceType :: !ServiceType
  , description :: !T.Text
  , stdOut :: !ServiceStream
  , stdErr :: !ServiceStream
  , execStart :: !FilePath
  , wantedBy :: ![FilePath]
  }

instance Show ServiceType where
  show :: ServiceType -> String
  show = \case
    Simple -> "simple"
    Exec -> "exec"
instance Show ServiceStream where
  show :: ServiceStream -> String
  show = \case
    Journal -> "journal"
    FileWrite path -> "file:" <> path

serviceFile :: Service -> String
serviceFile MkService{..} =
  unlines
    [ printf "[Unit]"
    , printf "Description=%s" description
    , printf "[Service]"
    , printf "Type=%s" (show serviceType)
    , printf "StandardOutput=%s" (show stdOut)
    , printf "StandardError=%s" (show stdErr)
    , printf "ExecStart=%s" execStart
    , printf "[Install]"
    , wantedByTxt
    ]
  where
    wantedByTxt = case wantedBy of
      [] -> ""
      xs -> printf "WantedBy=%s" (unwords xs)

-- | Denotes a string with shell variables embedded.
newtype ShellString = MkShellStr [ShellStrElem]
  deriving (Show)

data ShellStrElem = Str !T.Text | Var !T.Text
  deriving (Show)

-- | Example:
--
-- >>> parseShellString (T.pack "Hello, ${NAME}! ${GREETINGS}.")
-- MkShellStr [Str "Hello, ",Var "NAME",Str "! ",Var "GREETINGS",Str "."]
--
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

shellExpandWith :: (MonadFail m) => (T.Text -> m T.Text) -> ShellString -> m T.Text
shellExpandWith act (MkShellStr strs) = mconcat <$> traverse expand strs
  where
    expand = \case
      Str str -> pure str
      Var var -> act var

shellExpand :: ShellString -> IO T.Text
shellExpand = shellExpandWith (fmap T.pack . getEnv . T.unpack)
