module While.Parser where

import Data.Maybe
import While
import Text.Megaparsec
import Text.Megaparsec.Text

parseWhile :: Parser While
parseWhile = do
  w <- parseAssign <|> parseLoop
  parseSeq w
  where
    parseSeq w = option w $ Sequence w <$> (char ';' *> parseWhile)

    parseLoop :: Parser While
    parseLoop = undefined

parseVar :: Parser Var
parseVar = mkVar <$> some alphaNumChar

--TODO: lex input first (gets rid of space boilerplate and prevents errors)
parseAssign :: Parser While
parseAssign = try $ do
  varX <- parseVar
  space
  string ":="
  space
  varY <- parseVar
  space
  char '+'
  space
  
  negative <- isJust <$> optional (char '-')
  sNum <- some digitChar
  -- Usually read is a bad idea. But we're parsing an Integer and ensuring
  -- it can be read.
  let num = read sNum
  pure $ Assign varX varY (if negative then negate num else num)

-- Test with
-- %> cabal repl
-- *While> :l While.Parser
-- Note: usually parseTest requires Text here,
-- but :set -XOverloadedStrings is set automatically in ./.ghci
-- *While.Parser> parseTest parseAssign "x := x + 1"
-- Assign (Var "x") (Var "x") 1
