{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module EndoT where

-- TODO: attempt to replace w/ http://hackage.haskell.org/package/monoid-extras-0.4.2/docs/Data-Monoid-Endomorphism.html
--       once that is released w/ an update to work with base 4.14.x.x

import Control.Monad ((<=<))
import GHC.Generics (Generic)
import Control.Lens (Rewrapped, Wrapped)

newtype EndoT m a = EndoT { appEndoT :: a -> m a } deriving (Generic)

instance (Monad m) => Semigroup (EndoT m a) where
  EndoT a <> EndoT b = EndoT (a <=< b)

instance (Monad m) => Monoid (EndoT m a) where
  mempty = EndoT pure

instance Wrapped (EndoT m a) where
instance (t ~ EndoT n b) => Rewrapped (EndoT m a) t