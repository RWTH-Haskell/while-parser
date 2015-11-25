{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}
module While.Parser.Lexer where

import           Control.Monad
import           Text.Megaparsec
import qualified Text.Megaparsec.Lexer as L

spaceConsumer = L.space (void spaceChar) (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

lexeme = L.lexeme spaceConsumer
symbol = L.symbol spaceConsumer

semicolon = symbol ";"
while = symbol "WHILE"
unequalZero = symbol "!=" *> symbol "0"
do_ = symbol "DO"
end = symbol "END"
assignment = symbol ":="
plus = symbol "+"

integer = lexeme L.integer
signedInteger = L.signed spaceConsumer integer

identifier = lexeme (some alphaNumChar) <?> "identifier"
