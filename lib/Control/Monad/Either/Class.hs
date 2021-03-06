{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
import Data.Kind

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

-- Swap instances

newtype Swap m a b = Swap { unSwap :: m b a } deriving (Eq, Show)

instance (Functor f) => Functor (Swap (EitherT f) a) where
  fmap f (Swap efab) = Swap $ first f efab

instance (Applicative m) => Applicative (Swap (EitherT m) a) where
  pure = Swap . EitherT . pure . Left
  liftA2 f (Swap (EitherT efx)) (Swap (EitherT eax)) = Swap . EitherT $ f' <$> efx <*> eax
    where
      f' (Left a) (Left b) = Left $ f a b
      f' (Right x) _ = Right x
      f' _ (Right x) = Right x

instance (Monad m) => Monad (Swap (EitherT m) a) where
  (Swap (EitherT mae)) >>= f = Swap $ EitherT $ mae >>= \case
    (Right x) -> pure $ Right x
    (Left a) -> runEitherT (unSwap (f a))

instance (Functor f) => Functor (Swap (TupleT f) a) where
  fmap f (Swap efab) = Swap $ first f efab

instance (Monoid a, Applicative m) => Applicative (Swap (TupleT m) a) where
  pure = Swap . TupleT . pure . (,mempty)
  liftA2 f (Swap (TupleT efx)) (Swap (TupleT eax)) = Swap . TupleT $ f' <$> efx <*> eax
    where
      f' (a,x) (b,x') = (f a b, x <> x')

instance (Monoid a, Monad m) => Monad (Swap (TupleT m) a) where
  (Swap (TupleT mae)) >>= f = Swap $ TupleT $ mae >>= \case
    (a,x) -> runTupleT (unSwap (f a)) <&> \case
      (b, x') -> (b, x <> x')

instance (Functor f) => Functor (Swap (TheseT f) a) where
  fmap f (Swap efab) = Swap $ first f efab

instance (Monoid a, Applicative m) => Applicative (Swap (TheseT m) a) where
  pure = Swap . TheseT . pure . This
  liftA2 f (Swap (TheseT efx)) (Swap (TheseT eax)) = Swap . TheseT $ f' <$> efx <*> eax
    where
      f' (This a) (This b) = This (f a b)
      f' (This _) (That x') = That x'
      f' (This a) (These b x') = These (f a b) x'
      f' (That x) _ = That x
      f' (These a x) (This b) = These (f a b) x
      f' (These _ x) (That x') = That (x <> x')
      f' (These a x) (These b x') = These (f a b) (x <> x')

instance (Monoid a, Monad m) => Monad (Swap (TheseT m) a) where
  (Swap (TheseT mt)) >>= f = Swap $ TheseT $ mt >>= \case
    This a -> runTheseT . unSwap $ f a
    That b -> pure $ That b
    These a x -> runTheseT (unSwap (f a)) <&> \case
      This b -> These b x
      That x' -> That (x <> x')
      These b x' -> These b (x <> x')

-- class (Bifunctor m, forall e . Applicative (m e)) => MonadXApplicative m where


class (forall a . c a => Monad (m a), forall e . d e => Monad (Swap m e)) => ConjoinedMonad c d m where
  throw :: (d a) => e -> m e a
  default throw :: (d a) => e -> m e a
  throw e = unSwap $ pure e

  handle :: (d a) => m e a -> (e -> m f a) -> m f a
  default handle :: (d a) => m e a -> (e -> m f a) -> m f a
  cm `handle` f = unSwap $ Swap cm >>= Swap . f

class Empty x
instance Empty x

instance (Monad m) => ConjoinedMonad Empty Empty (EitherT m) where
instance (Monad m) => ConjoinedMonad Monoid Monoid (TupleT m) where
instance (Monad m) => ConjoinedMonad Monoid Monoid (TheseT m) where

-- instance ConjoinedMonad Either where
--   throw = Left
--   (Left e) `handle` f = f e
--   (Right a) `handle` _ = Right a

-- instance (Monad m) => ConjoinedMonad (EitherT m) where
--   throw = EitherT . pure . Left
--   (EitherT met) `handle` f = EitherT $ met >>= \case
--     (Left e) -> runEitherT $ f e
--     (Right a) -> pure $ Right a

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