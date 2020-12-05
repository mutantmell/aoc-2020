{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}

module Control.Monad.Either.Class
  ( MonadEither(..)
  , MonadConjoinedTrans(..)

  , EitherT(..)
  , Swap(..)
  ) where

import Control.Applicative
import Data.Functor.Classes

newtype EitherT m e a = EitherT
  { runEitherT :: m (Either e a)
  } deriving (Functor)

instance (Eq1 m, Eq e) => Eq1 (EitherT m e) where
  liftEq eq (EitherT x) (EitherT y) = liftEq (liftEq eq) x y

instance (Eq1 m, Eq e, Eq a) => Eq (EitherT m e a) where
  (==) = eq1

instance (Show1 m, Show e) => Show1 (EitherT m e) where
  liftShowsPrec sp sl d (EitherT m) = 
    showsUnaryWith (liftShowsPrec sp' sl') "EitherT" d m
      where
        sp' = liftShowsPrec sp sl
        sl' = liftShowList sp sl

instance (Show1 m, Show e, Show a) => Show (EitherT m e a) where
  showsPrec = showsPrec1

instance (Applicative m) => Applicative (EitherT m e) where
  pure = EitherT . pure . Right

  (EitherT efa) <*> (EitherT ea) = EitherT $ (<*>) <$> efa <*> ea
  liftA2 f (EitherT ea) (EitherT eb) = EitherT $ liftA2 (liftA2 f) ea eb

instance (Monad m) => Monad (EitherT m e) where
  (EitherT met) >>= f = EitherT $ met >>= \case
    (Left e) -> pure $ Left e
    (Right a) -> runEitherT $ f a

-- add a potential constraint on throw/handle methods?

-- class (forall e . Applicative (m e)) => ApplicativeEither m where

class (forall e . Monad (m e)) => MonadEither m where
  throw ::  e -> m e a
  handle :: m e a -> (e -> m f a) -> m f a

newtype Swap m a b = Swap { unSwap :: m b a } deriving (Eq, Show)

instance (MonadEither m) => Functor (Swap m a) where
  fmap f (Swap m) = Swap $ m `handle` (throw . f)

instance (MonadEither m) => Applicative (Swap m a) where
  pure = Swap . throw
  (Swap mf) <*> (Swap ma) = Swap $ mf `handle` \f -> ma `handle` \a -> throw (f a)

instance (MonadEither m) => Monad (Swap m a) where
  (Swap ma) >>= f = Swap $ ma `handle` (unSwap . f)

instance (MonadEither m) => MonadEither (Swap m) where
  throw = Swap . pure
  (Swap ma) `handle` f = Swap $ ma >>= (unSwap . f)

instance MonadEither Either where
  throw = Left
  handle (Left e) f = f e
  handle (Right a) _ = Right a

instance (Monad m) => MonadEither (EitherT m) where
  throw = EitherT . pure . Left
  handle (EitherT met) f = EitherT $ met >>= \case
    (Left e) -> runEitherT $ f e
    (Right a) -> pure $ Right a

instance (forall a . Monoid a) => MonadEither (,) where
  throw e = (e, mempty)
  handle (e,a) f = let (e',a') = f e in (e', a <> a') 

class MonadConjoinedTrans t where
  liftLeft :: (Monad m) => m a -> t m a b
  liftRight :: (Monad m) => m b -> t m a b

instance MonadConjoinedTrans EitherT where
  liftLeft = EitherT . fmap Left
  liftRight = EitherT . fmap Right

--instance (forall m . MonadTrans (EitherT m e)) where
--  lift = undefined