{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}

{-
An implementation of some of the findings in Exceptionally Monadic Error Handling

https://arxiv.org/pdf/1810.13430.pdf
-}

module Control.Monad.Either.Class
  ( ConjoinedMonad(..)
  , ConjoinedMonadTrans(..)

  , EitherT(..)
  , TupleT(..)

  , Swap(..)
  ) where

import Control.Applicative (Applicative(liftA2))
import Data.Functor.Classes (eq1, showsPrec1, showsUnaryWith, Eq1(..), Show1(..))
import Data.Bifunctor (Bifunctor(..))

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

instance (Functor f) => Bifunctor (EitherT f) where
  bimap f g (EitherT me) = EitherT $ fmap (bimap f g) me
  first f (EitherT me) = EitherT $ fmap (first f) me
  second g (EitherT me) = EitherT $ fmap (second g) me

instance (Applicative m) => Applicative (EitherT m e) where
  pure = EitherT . pure . Right

  (EitherT efa) <*> (EitherT ea) = EitherT $ (<*>) <$> efa <*> ea
  liftA2 f (EitherT ea) (EitherT eb) = EitherT $ liftA2 (liftA2 f) ea eb

instance (Monad m) => Monad (EitherT m e) where
  (EitherT met) >>= f = EitherT $ met >>= \case
    (Left e) -> pure $ Left e
    (Right a) -> runEitherT $ f a

newtype TupleT m w a = TupleT
  { runTupleT :: m (w, a)
  } deriving (Functor)

instance (Eq1 m, Eq w) => Eq1 (TupleT m w) where
  liftEq eq (TupleT x) (TupleT y) = liftEq (liftEq eq) x y

instance (Eq1 m, Eq w, Eq a) => Eq (TupleT m w a) where
  (==) = eq1

instance (Show1 m, Show w) => Show1 (TupleT m w) where
  liftShowsPrec sp sl d (TupleT m) = 
    showsUnaryWith (liftShowsPrec sp' sl') "TupleT" d m
      where
        sp' = liftShowsPrec sp sl
        sl' = liftShowList sp sl

instance (Show1 m, Show w, Show a) => Show (TupleT m w a) where
  showsPrec = showsPrec1

instance (Functor f) => Bifunctor (TupleT f) where
  bimap f g (TupleT me) = TupleT $ fmap (bimap f g) me
  first f (TupleT me) = TupleT $ fmap (first f) me
  second g (TupleT me) = TupleT $ fmap (second g) me

instance (Applicative m, Monoid w) => Applicative (TupleT m w) where
  pure = TupleT . pure . (mempty,)

  (TupleT tfa) <*> (TupleT ta) = TupleT $ (<*>) <$> tfa <*> ta
  liftA2 f (TupleT ta) (TupleT tb) = TupleT $ liftA2 (liftA2 f) ta tb

instance (Monad m, Monoid w) => Monad (TupleT m w) where
  (TupleT met) >>= f = TupleT $ met >>= \(w,a) ->
    runTupleT (f a) >>= \(w',b) ->
      pure (w <> w', b)

-- class (Bifunctor m, forall e . Applicative (m e)) => MonadXApplicative m where

-- could split this out into a ConjoinedApplicative and a ConjoinedMonad
class (Bifunctor m, forall e . Monad (m e)) => ConjoinedMonad m where
  throw :: e -> m e a
  handle :: m e a -> (e -> m f a) -> m f a

instance ConjoinedMonad Either where
  throw = Left
  (Left e) `handle` f = f e
  (Right a) `handle` _ = Right a

instance (Monad m) => ConjoinedMonad (EitherT m) where
  throw = EitherT . pure . Left
  (EitherT met) `handle` f = EitherT $ met >>= \case
    (Left e) -> runEitherT $ f e
    (Right a) -> pure $ Right a

instance (forall a . Monoid a) => ConjoinedMonad (,) where
  throw e = (e, mempty)
  (e,a) `handle` f = let (e',a') = f e in (e', a <> a')

instance (Monad m, forall a . Monoid a) => ConjoinedMonad (TupleT m) where
  throw = TupleT . pure . (,mempty)
  (TupleT mt) `handle` f = TupleT $ mt >>= \(w,a) ->
    runTupleT (f w) >>= \(x, a') ->
      pure (x, a <> a')

class ConjoinedMonadTrans t where
  liftLeft :: (Monad m) => m a -> t m a b
  liftRight :: (Monad m) => m b -> t m a b

instance ConjoinedMonadTrans EitherT where
  liftLeft = EitherT . fmap Left
  liftRight = EitherT . fmap Right

instance (forall a . Monoid a, forall b . Monoid b) => ConjoinedMonadTrans TupleT where
  liftLeft = TupleT . fmap (,mempty)
  liftRight = TupleT . fmap (mempty,)

newtype Swap m a b = Swap { unSwap :: m b a } deriving (Eq, Show)

instance (ConjoinedMonad m) => Functor (Swap m a) where
  fmap f (Swap m) = Swap $ first f m

instance (ConjoinedMonad m) => Bifunctor (Swap m) where
  bimap f g (Swap m) = Swap $ bimap g f m
  first f (Swap m) = Swap $ second f m
  second g (Swap m) = Swap $ first g m

instance (ConjoinedMonad m) => Applicative (Swap m a) where
  pure = Swap . throw
  (Swap mf) <*> (Swap ma) = Swap $ mf `handle` \f -> ma `handle` \a -> throw (f a)

instance (ConjoinedMonad m) => Monad (Swap m a) where
  (Swap ma) >>= f = Swap $ ma `handle` (unSwap . f)

instance (ConjoinedMonad m) => ConjoinedMonad (Swap m) where
  throw = Swap . pure
  (Swap ma) `handle` f = Swap $ ma >>= (unSwap . f)
