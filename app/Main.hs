module Main where

import EitherM

isEven :: Int -> EitherM Int
isEven n
  | n `mod` 2 == 0 = return n
  | otherwise = fail "is not even!"

useMonad :: Int -> Int -> EitherM Int
useMonad n m = do
  nResult <- isEven n
  mResult <- isEven m
  return $ nResult + mResult

main :: IO ()
main = print $ useMonad 2 4
