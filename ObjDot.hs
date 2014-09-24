module ObjDot where

the = flip id

test :: [(Int,Int,Int)]
test =
  the([1..10]).concatMap $ \a ->
    the([1..10]).concatMap $ \b ->
      the([1..10]).concatMap $ \c ->
        if a^2+b^2 == c^2
          then [(a,b,c)]
          else []

test2 :: [String]
test2 =
  the(test).map $ \(a,b,c) ->
    show a ++ "^2 + " ++ show b ++ "^2 = " ++ show c
