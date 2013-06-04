{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, KindSignatures, ExistentialQuantification, FlexibleInstances, RankNTypes, LiberalTypeSynonyms #-}
module Control.Channel (Channel,
                        ChannelT,
                        MonadChannel(..),
                        module Control.Monad.Trans,
                        module Control.Monad.Identity,
                        module Control.Monad.Base,
                        module Control.Monad.Morph,
                        -- * Internals
                        ChannelF(..)) where
import Control.Monad.Trans
import Control.Monad.Identity
import Control.Monad.Base
import Control.Monad.Morph
import Control.Applicative (Applicative(..))
import Control.Monad.Trans.Free.Instances

type ChannelT sel = FreeT (ChannelF sel)

data ChannelF (sel :: * -> * -> *) (x :: *) = forall (i :: *) (o :: *). SyncChannel (sel i o) o (i -> x)

instance Functor (ChannelF sel) where
  fmap f (SyncChannel s o i) = SyncChannel s o (f . i)

class (Monad m, Applicative m) => MonadChannel (sel :: * -> * -> *) (m :: * -> *) | m -> sel where
  syncOn :: sel i o -> o -> m i

instance (Monad m) => MonadChannel sel (FreeT (ChannelF sel) m) where
  syncOn s o = FreeT . return . Free . SyncChannel s o $ FreeT . return . Pure

type Channel sel a = forall (m :: * -> *). (MonadChannel sel m) => m a
