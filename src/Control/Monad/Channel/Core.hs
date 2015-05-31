{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, KindSignatures #-}
{-# LANGUAGE ExistentialQuantification, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving, GeneralizedNewtypeDeriving, Trustworthy #-}
module Control.Monad.Channel.Core (Channel,
                                   ChannelT(..),
                                   MonadChannel(..),
                                   module Control.Monad.Trans,
                                   module Control.Monad.Identity,
                                   module Control.Monad.Base,
                                   module Control.Monad.Morph,
                                   -- * Internals
                                   ChannelF(..),
                                   module Control.Monad.Trans.Free) where
import Control.Monad.Trans
import Control.Monad.Identity
import Control.Monad.Base
import Control.Monad.Morph
import Control.Monad.Trans.Free

instance (MonadBase b m, Functor f) => MonadBase b (FreeT f m) where
  liftBase = liftBaseDefault

instance (Functor f) => MFunctor (FreeT f) where
  hoist = hoistFreeT

newtype ChannelT sel m a = ChannelT { unChannelT :: FreeT (ChannelF sel) m a }
  deriving (Functor, Applicative, Monad, MFunctor, MonadTrans)
deriving instance (MonadBase b m) => MonadBase b (ChannelT sel m)

data ChannelF (sel :: * -> * -> *) (x :: *) = forall (i :: *) (o :: *). SyncChannel
  { selectorF :: sel i o,
    outputF :: o,
    inputF :: i -> x }

instance Functor (ChannelF sel) where
  fmap f (SyncChannel s o i) = SyncChannel s o (f . i)

class (Monad m) => MonadChannel (sel :: * -> * -> *) (m :: * -> *) | m -> sel where
  syncOn :: sel i o -> o -> m i

instance (Monad m) => MonadChannel sel (ChannelT sel m) where
  syncOn s o = ChannelT . FreeT . return . Free . SyncChannel s o $ FreeT . return . Pure

type Channel sel = ChannelT sel Identity
