{-# LANGUAGE FunctionalDependencies, PolyKinds, FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE AutoDeriveTypeable, DeriveFunctor, DeriveGeneric #-}
module Control.Monad.Channel.Internal (ChannelT(..), Channel, MonadChannel(..), ChannelF(..)) where
import Control.Monad.Trans
import Control.Monad.Identity
import Control.Monad.Base
import Control.Monad.Morph
import Control.Monad.Trans.Free
import GHC.Generics

instance (MonadBase b m, Functor f) => MonadBase b (FreeT f m) where
  liftBase = liftBaseDefault
instance Functor f => MFunctor (FreeT f) where
  hoist = hoistFreeT
deriving instance Generic (FreeT f m a)

newtype ChannelT sel m a = ChannelT { unChannelT :: FreeT (ChannelF sel) m a }
  deriving (Functor, Applicative, Monad, MonadTrans, MonadFree (ChannelF sel), Generic1, Generic)
deriving instance MonadBase b m => MonadBase b (ChannelT sel m)

data ChannelF (sel :: * -> * -> *) (x :: *) =
  forall (i :: *) (o :: *). SyncChannel {
    selectorF :: sel i o,
    outputF :: o,
    inputF :: i -> x
  }
deriving instance Functor (ChannelF sel)

class Monad m => MonadChannel (sel :: * -> * -> *) (m :: * -> *) | m -> sel where
  syncOn :: sel i o -> o -> m i

instance Monad m => MonadChannel sel (ChannelT sel m) where
  syncOn s o = ChannelT . FreeT . return . Free . SyncChannel s o $ FreeT . return . Pure

-- NB: Don't use GeneralizedNewtypeDeriving to create this instance, as it will
-- trigger GHC Trac #11837 on GHC 8.0.1 and older.
instance MFunctor (ChannelT sel) where
  hoist f = ChannelT . hoist f . unChannelT

type Channel sel = ChannelT sel Identity
