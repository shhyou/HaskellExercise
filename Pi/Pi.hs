{-# LANGUAGE FlexibleContexts #-}

module Pi where

import Control.Monad
import Control.Monad.Except

type Err = String

data Term = Var Int
          | Ap Term Term

          | Lam String Term         -- check

          | Pi String Term Term     -- types

          | U

          deriving (Show, Eq)

type Context = [(String, Term)]

contextLookup :: Context -> Int -> Value
contextLookup = (snd .) . (!!)

contextExtend :: String -> Value -> Context -> Context
contextExtend = ((:) .) . (,)

data Value = Fun (Value -> Value)



