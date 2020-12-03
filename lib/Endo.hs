{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module Endo where

-- replace w/ http://hackage.haskell.org/package/monoid-extras-0.4.2/docs/Data-Monoid-Endomorphism.html
-- once that is released w/ an update to work with base 4.14.x.x

import GHC.Generics (Generic)
import Control.Lens.Wrapped (Rewrapped, Wrapped)
import Control.Category (Category, (<<<))
import qualified Control.Category as Category

newtype Endo k a = Endo { appEndo :: a `k` a } deriving (Generic)

deriving instance (Show (a `k` a)) => Show (Endo k a)

instance (Category k) => Semigroup (Endo k a) where
  Endo a <> Endo b = Endo (a <<< b)

instance (Category k) => Monoid (Endo k a) where
  mempty = Endo Category.id

instance Wrapped (Endo m a)
instance (t ~ Endo n b) => Rewrapped (Endo m a) t
