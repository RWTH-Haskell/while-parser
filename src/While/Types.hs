module While.Types where

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
