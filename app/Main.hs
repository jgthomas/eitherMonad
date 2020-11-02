module Main where

import EitherM
import System.Environment (getArgs)

isEven :: Int -> EitherM Int
isEven n
  | n `mod` 2 == 0 = return n
  | otherwise = fail $ show n ++ " is not even!"

isLessThanTen :: Int -> EitherM Int
isLessThanTen n
  | n < 10 = return n
  | otherwise = fail $ show n ++ " is not less than 10!"

-- arguments to >>=
-- 1. isEven n -> m a
-- 2. (\n -> isEven m >>= (\m -> return $ n + m)) -- a lambda going from a -> m b
-- a == n; m b = return (n + m) = EitherM (Right b)
--
-- power of the monad
-- 1) Sequences the operations, making sure isEven is checked before isLessThan.
--    But this could be done with an Applicative, by putting all the functions in a
--    context and applying them sequentially. No need for Monad power here!
-- 2) Defines what happens if one of the checks fails, as part of the definition
--    of >>=. This is the true power of the monad, and what it adds beyond an
--    Applicative.
numberPasses :: Int -> EitherM Int
numberPasses n = isEven n >>= (\n -> isLessThanTen n >>= (\n -> return n))

numberPassesDo :: Int -> EitherM Int
numberPassesDo n = do
  evenResult <- isEven n
  sizeResult <- isLessThanTen n
  return n

number :: Int
number = 18

main :: IO ()
main = do
  args <- getArgs
  print "Desugared"
  print $ numberPasses . read . head $ args
  print "Sugared"
  print $ numberPassesDo . read . head $ args
