-- | Configurations based on haskell syntax
module Config
  ( Parsec,
    ParseError,
    parse,
    fieldP,
    recordP,
    textP,
    pathP,
    maybeP,
    boolP,
    completeP,
    commaP,
    mapP,
    identP,
  )
where

import Common
import Control.Monad.Identity
import Data.Map qualified as M
import Data.Text qualified as T
import Text.Parsec
import Text.Parsec.Token qualified as P
import Text.Printf

eqSign = P.reservedOp cfgLang "="

expectP str = try $ do s <- P.identifier cfgLang; guard (s == str)

-- MAYBE make read record less typed?

completeP :: Parsec T.Text () a -> Parsec T.Text () a
completeP p = P.whiteSpace cfgLang *> p <* eof

recordP :: String -> Parsec T.Text () a -> Parsec T.Text () a
recordP rname p = expectP rname *> P.braces cfgLang p <?> printf "Record %s" rname

commaP :: Parsec T.Text () String
commaP = P.comma cfgLang

fieldP :: String -> Parsec T.Text () a -> Parsec T.Text () a
fieldP fname p = expectP fname *> eqSign *> p <?> printf "Field %s" fname

pathP :: Parsec T.Text () FilePath
pathP = P.stringLiteral cfgLang

textP :: Parsec T.Text () T.Text
textP = T.pack <$> P.stringLiteral cfgLang

identP :: Parsec T.Text () ID
identP = P.stringLiteral cfgLang >>= makeIDM

maybeP :: Parsec T.Text () a -> Parsec T.Text () (Maybe a)
maybeP p = Just <$> (expectP "Just" *> p) <|> Nothing <$ expectP "Nothing"

-- MAYBE generic implement for ADT
boolP :: Parsec T.Text () Bool
boolP = False <$ expectP "False" <|> True <$ expectP "True"

mapP :: Parsec T.Text () a -> Parsec T.Text () (M.Map String a)
mapP p = M.fromList <$> P.squares cfgLang (P.commaSep cfgLang kvPair)
  where
    kvPair = (,) <$> P.identifier cfgLang <*> (eqSign *> p)

cfgLang :: P.GenTokenParser T.Text () Identity
cfgLang = P.makeTokenParser cfgLangDef
  where
    cfgLangDef =
      P.LanguageDef
        { commentStart = "{-",
          commentEnd = "-}",
          commentLine = "#",
          nestedComments = True,
          identStart = letter,
          identLetter = alphaNum <|> oneOf "_'",
          opStart = P.opLetter cfgLangDef,
          opLetter = oneOf "=",
          reservedNames = [],
          reservedOpNames = ["="],
          caseSensitive = True
        }
