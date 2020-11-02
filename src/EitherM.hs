module EitherM where

import Control.Applicative (Applicative)
import Control.Monad (ap, liftM)
import Control.Monad.Fail as Fail

newtype EitherM a = EitherM (Either String a)
    deriving (Show)

instance Functor EitherM where
  fmap = liftM

instance Applicative EitherM where
  pure = return
  (<*>) = ap

instance Monad EitherM where
  return a = EitherM (Right a)

  (EitherM em) >>= k =
    case em of
      Left msg -> EitherM (Left msg)
      Right a -> k a

instance Fail.MonadFail EitherM where
  fail msg = EitherM (Left msg)
