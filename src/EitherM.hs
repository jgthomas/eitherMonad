module EitherM where

import Control.Applicative (Applicative)
import Control.Monad (ap, liftM)
import Control.Monad.Fail (MonadFail)

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

-- fail has been removed from the Monad
-- class as it didn't make much sense for
-- lots of Monads, so now has to be defined
-- separately for those Monads that require it
instance MonadFail EitherM where
  fail msg = EitherM (Left msg)
