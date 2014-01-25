{-# LANGUAGE RankNTypes, GADTs, ScopedTypeVariables, TypeOperators,
             FlexibleContexts, FlexibleInstances #-}
module TestEffect where

import Effect
import Control.Monad (liftM2, when)

{- tests -}

testS :: Member (State Int) r => Eff r Int
testS = do
  n :: Int <- get
  modify (+(1 ::Int))
  m :: Int <- get
  modify (*(2 :: Int))
  k :: Int <- get
  return (n*m*k)

testR :: Member (Reader Int) r => Eff r Int
testR = do
  n :: Int <- ask
  return $ 3 * n + 1

testR2 :: Member (Reader Int) r => Eff r Int
testR2 = local (+1) testR

testRP = run $ runReader testR (5 :: Int)
testR2P = run $ runReader testR2 (5 :: Int)

testE :: (Member (Error String) r, Num a, Ord a) => a -> Eff r ()
testE m = do
  if (m > 10)
    then throwError "m > 10"
    else return True
  return ()

testEP :: (Num a, Ord a) => a -> Either String ()
testEP m = run $ runError (testE m)

testC = do
  x <- choice [1..3]
  y <- choice [1..4]
  return (x,y)

testCP = run $ runChoice testC

testAll :: (Member (Error String) r, Member (Reader Int) r, Member Choice r) => Eff r (Int, Int)
testAll = do
  up :: Int <- local (*2) testR
  x :: Int <- choice [1..up]
  when (x > 10) (throwError "x > 10")
  y :: Int <- choice [1..3]
  return (x,y)

concreteTest :: Eff (Reader Int :> Error String :> Choice :> Void) (Int, Int)
concreteTest = testAll

concreteTestP = run $ runChoice $ runError $ runReader concreteTest (2 :: Int)

-- https://groups.google.com/forum/#!topic/haskell-pipes/BTQsITNwflc
-- by Gabriel Gonzalez
example :: (Member (Error String) r, Member (State Int) r) => Eff r Int
example = do 
    put (1 :: Int)
    (do put (2 :: Int)
        throwError "Err") `catchError` (\(_ :: String) -> return ())
    get

-- runExample1 = runExample2
runExample1 :: Either String Int
runExample1 = run $ runError $ runState (0 :: Int) example

runExample2 :: Either String Int
runExample2 = run $ runState (0 :: Int) $ runError example
