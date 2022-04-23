module Profile where

import Checked
import Control.Applicative
import Control.Exception hiding (try)
import Control.Monad
import Control.Monad.Identity
import Data.Text qualified as T
import Data.Text.IO qualified as T
import System.Directory
import System.FilePath
import Text.Parsec (ParseError, Parsec, alphaNum, letter, oneOf, parse, try, (<?>))
import Text.Parsec.Combinator
import Text.Parsec.Token
import Text.Printf

-- | Profile Config
data ProfileCfg = ProfileCfg
  { profileID :: !ID,
    profileName :: !T.Text,
    installScript :: !(Maybe FilePath)
  }
  deriving (Read, Show)

-- | Profile. Requires the config path to exist.
data Profile = Profile
  { profCfg :: !ProfileCfg,
    cfgDir, dataDir, cacheDir, logDir :: !FilePath,
    starter :: !FilePath
  }

data ProfileError
  = ProfileNotFound ID
  | ProfileWrongFormat ID String
  deriving (Show)

instance Exception ProfileError

parseCfg :: T.Text -> Either ParseError ProfileCfg
parseCfg = parse parserCfg "profile.cfg"
  where
    parserCfg :: Parsec T.Text () ProfileCfg
    parserCfg =
      whiteSpace cfgLang *> expect "ProfileCfg"
        *> (braces cfgLang)
          ( ProfileCfg
              <$> field "profileID" parserID
              <*> (comma cfgLang *> field "profileName" parserText)
              <*> (comma cfgLang *> field "installScript" (parserMaybe parserPath))
          ) <* eof
          <?> "Profile configuration"

    expect str = try $ do s <- identifier cfgLang; guard (s == str)
    field str p = expect str *> reservedOp cfgLang "=" *> p <?> printf "Field %s" str
    parserID = stringLiteral cfgLang >>= makeIDM
    parserText = T.pack <$> stringLiteral cfgLang -- MAYBE not rely on String
    parserMaybe p = expect "Just" *> (Just <$> p) <|> Nothing <$ expect "Nothing"
    parserPath = stringLiteral cfgLang

    cfgLang :: GenTokenParser T.Text () Identity
    cfgLang = makeTokenParser cfgLangDef
    cfgLangDef =
      LanguageDef
        { commentStart = "{-",
          commentEnd = "-}",
          commentLine = "#",
          nestedComments = True,
          identStart = letter,
          identLetter = alphaNum,
          opStart = opLetter cfgLangDef,
          opLetter = oneOf "=",
          reservedNames = [],
          reservedOpNames = ["="],
          caseSensitive = True
        }

-- | Gets a profile from specified path.
getProfileFromPath :: FilePath -> ID -> FilePath -> IO Profile
getProfileFromPath project profID cfgDir = do
  doesDirectoryExist cfgDir >>= (`unless` throwIO (ProfileNotFound profID))
  profCfg <-
    parseCfg <$> T.readFile (cfgDir </> "profile.cfg") >>= \case
      Left err -> throwIO (ProfileWrongFormat profID $ show err)
      Right cfg -> pure cfg
  pure (Profile {profCfg, cfgDir, dataDir, cacheDir, logDir, starter})
  where
    dataDir = project </> "data" </> idStr profID
    cacheDir = project </> "cache" </> idStr profID
    logDir = project </> "logs" </> idStr profID
    starter = project </> "start.sh"
