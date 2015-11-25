module While.Parser where

import Control.Applicative
import Data.Maybe
import Text.Megaparsec
import Text.Megaparsec.Text
import While.Types
import While.Parser.Lexer

parseWhile :: Parser While
parseWhile = do
  w <- parseAssign <|> parseLoop
  parseSeq w
  where
    parseSeq w = option w . liftA (Sequence w) $ semicolon *> parseWhile

parseVar :: Parser Var
parseVar = mkVar <$> identifier

--TODO: lex input first (gets rid of space boilerplate and prevents errors)
parseAssign :: Parser While
parseAssign = try $ do
  varX <- parseVar
  assignment
  varY <- parseVar
  plus
  num <- signedInteger
  pure $ Assign varX varY num

parseLoop :: Parser While
parseLoop = do
  while -- parsing will fail here if we're assigning, so no need to worry
        -- on that topic, add a list of reserved identifiers
  var <- parseVar
  unequalZero
  do_
  whileP <- parseWhile
  pure $ Loop var whileP
  
-- Test with
-- %> cabal repl
-- *While> :l While.Parser
-- Note: usually parseTest requires Text here,
-- but :set -XOverloadedStrings is set automatically in ./.ghci
-- *While.Parser> parseTest parseWhile "x := x + 1"
-- Assign (Var "x") (Var "x") 1
