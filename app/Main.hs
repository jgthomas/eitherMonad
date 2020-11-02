module Main where

import EitherM

isEven :: Int -> EitherM Int
isEven n
  | n `mod` 2 == 0 = return n
  | otherwise = fail $ show n ++ " is not even!"

allEven :: Int -> Int -> EitherM Int
allEven n m = isEven n >>= (\n -> isEven m >>= (\m -> return m))

allEvenDo :: Int -> Int -> EitherM Int
allEvenDo n m = do
  nResult <- isEven n
  mResult <- isEven m
  return $ nResult + mResult

first :: Int
first = 2

second :: Int
second = 6

main :: IO ()
main = do
  print "Desugared"
  print $ allEven first second
  print "Sugared"
  print $ allEvenDo first second
