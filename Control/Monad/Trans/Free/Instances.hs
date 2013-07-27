{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, KindSignatures, ExistentialQuantification, FlexibleInstances, RankNTypes, LiberalTypeSynonyms, UndecidableInstances #-}
module Control.Monad.Trans.Free.Instances (FreeT(..), FreeF(..)) where
import Control.Monad.Trans
import Control.Monad.Base
import Control.Monad.Morph
import Control.Monad.Trans.Free

instance (MonadBase b m, Functor f) => MonadBase b (FreeT f m) where
  liftBase = liftBaseDefault

instance (Functor f) => MFunctor (FreeT f) where
  hoist = hoistFreeT
