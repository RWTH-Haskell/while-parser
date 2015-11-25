{-# LANGUAGE OverloadedStrings #-}
module While where

import Control.Applicative
import While.Types

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


