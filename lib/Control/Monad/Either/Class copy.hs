{-# LANGUAGE TypeFamilies #-}
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
  , TheseT(..)

  , Swap(..)
  ) where

import Control.Applicative (Applicative(liftA2))
import Data.Functor.Classes (eq1, showsPrec1, showsUnaryWith, Eq1(..), Show1(..))
import Data.Bifunctor (Bifunctor(..))
import Data.Functor ((<&>))
import Data.These (These(..))
import Data.Coerce

-- EitherT and Instances

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

-- TupleT and Instances

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
  bimap f g (TupleT mt) = TupleT $ fmap (bimap f g) mt
  first f (TupleT mt) = TupleT $ fmap (first f) mt
  second g (TupleT mt) = TupleT $ fmap (second g) mt

instance (Applicative m, Monoid w) => Applicative (TupleT m w) where
  pure = TupleT . pure . (mempty,)

  (TupleT tfa) <*> (TupleT ta) = TupleT $ (<*>) <$> tfa <*> ta
  liftA2 f (TupleT ta) (TupleT tb) = TupleT $ liftA2 (liftA2 f) ta tb

instance (Monad m, Monoid w) => Monad (TupleT m w) where
  (TupleT mt) >>= f = TupleT $ mt >>= \(w,a) ->
    runTupleT (f a) >>= \(w',b) ->
      pure (w <> w', b)

-- TheseT and instances

newtype TheseT m a b = TheseT { runTheseT :: m (These a b) } deriving (Functor)

instance (Eq1 m, Eq a) => Eq1 (TheseT m a) where
  liftEq eq (TheseT x) (TheseT y) = liftEq (liftEq eq) x y

instance (Eq1 m, Eq w, Eq a) => Eq (TheseT m w a) where
  (==) = eq1

instance (Show1 m, Show w) => Show1 (TheseT m w) where
  liftShowsPrec sp sl d (TheseT m) =
    showsUnaryWith (liftShowsPrec sp' sl') "TheseT" d m
      where
        sp' = liftShowsPrec sp sl
        sl' = liftShowList sp sl

instance (Show1 m, Show w, Show a) => Show (TheseT m w a) where
  showsPrec = showsPrec1

instance (Functor f) => Bifunctor (TheseT f) where
  bimap f g (TheseT mt) = TheseT $ fmap (bimap f g) mt
  first f (TheseT mt) = TheseT $ fmap (first f) mt
  second g (TheseT mt) = TheseT $ fmap (second g) mt

instance (Applicative m, Monoid w) => Applicative (TheseT m w) where
  pure = TheseT . pure . That

  (TheseT tfa) <*> (TheseT ta) = TheseT $ (<*>) <$> tfa <*> ta
  liftA2 f (TheseT ta) (TheseT tb) = TheseT $ liftA2 (liftA2 f) ta tb

instance (Monad m, Monoid w) => Monad (TheseT m w) where
  (TheseT mt) >>= f = TheseT $ mt >>= \case
    This a -> pure $ This a
    That b -> runTheseT (f b)
    These a b -> runTheseT (f b) <&> \case
      This a' -> This (a <> a')
      That c -> These a c
      These a' c -> These (a <> a') c

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

{-
instance (forall m w a . m w a ~ (w,a) => Monoid a) => ConjoinedMonad (,) where
  throw e = (e, mempty)
  (e,a) `handle` f = let (e',a') = f e in (e', a <> a')

instance (Monad m, forall m w a . m w a ~ (w,a) => Monoid a) => ConjoinedMonad (TupleT m) where
  throw = TupleT . pure . (,mempty)
  (TupleT mt) `handle` f = TupleT $ mt >>= \(w,a) ->
    runTupleT (f w) >>= \(x, a') ->
      pure (x, a <> a')

instance (forall m w a . m w a ~ These w a => Monoid a) => ConjoinedMonad These where
  throw = This

  (This a) `handle` f = f a
  (That b) `handle` _ = That b
  (These a b) `handle` f = case f a of
    This a' -> These a' b
    That b' -> That (b <> b')
    These a' b' -> These a' (b <> b')
-}

class ConjoinedMonadTrans t where
  liftLeft :: (Monad m) => m a -> t m a b
  liftRight :: (Monad m) => m b -> t m a b

instance ConjoinedMonadTrans EitherT where
  liftLeft = EitherT . fmap Left
  liftRight = EitherT . fmap Right

{-
instance (forall a . Monoid a, forall b . Monoid b) => ConjoinedMonadTrans TupleT where
  liftLeft = TupleT . fmap (,mempty)
  liftRight = TupleT . fmap (mempty,)
-}
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
