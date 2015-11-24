{-# LANGUAGE OverloadedStrings #-}
module While where

import Control.Applicative
import           Data.Text (Text)
import qualified Data.Text as T

newtype Var = Var Text
            deriving (Show, Eq)

mkVar :: String -> Var
mkVar = Var . T.pack

-- |
-- Simple imperative language with three operations
-- for all (x y : Var) (c : Integer), `x := y + c` is a While program
-- for all (w1 w2 : While), 'w1;w2' : While
-- for all (x : Var) (w : While), 'WHILE x != 0 DO w END' : While
data While
     = Assign Var Var Integer
     | Sequence While While
     | Loop Var While
     deriving (Show, Eq)

(<+>) :: (Applicative f, Num a) => f a -> f a -> f a
(<+>) = liftA2 (+)

eval :: (Var -> Maybe Integer) -> While -> Var -> Maybe Integer
eval value (Assign x y c) var | var == x  = value y <+> pure c
                              | otherwise = value var
eval value (Sequence w1 w2) var = eval (eval value w1) w2 var
eval value l@(Loop varX w) var = do
  x <- value varX
  if x /= 0 then eval (eval value w) l var else value var

-- |
-- WHILE y != 0 DO x := x + 1; y := y - 1 END
add :: While
add = Loop varY $ Sequence (Assign varX varX 1) (Assign varY varY (-1))
  where
    varX = Var "x"
    varY = Var "y"


