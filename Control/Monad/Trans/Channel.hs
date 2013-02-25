{-# LANGUAGE KindSignatures, ExistentialQuantification, RankNTypes, LiberalTypeSynonyms #-}
module Control.Monad.Trans.Channel (ChannelT,
                                    syncOn,
                                    -- * Internals
                                    ChannelF(..)) where
import Control.Monad.Trans
import Control.Monad.Base
import Control.Monad.Trans.Free (FreeT(..), FreeF(..))
import Control.Applicative (Applicative(..))

type ChannelT sel = FreeT (ChannelF sel)

data ChannelF sel x = forall (i :: *) (o :: *). SyncChannel (sel i o) o (i -> x)

instance Functor (ChannelF sel) where
  fmap f (SyncChannel s o i) = SyncChannel s o (f . i)

syncOn :: (Applicative m) => sel i o -> o -> ChannelT sel m i
syncOn s o = FreeT . pure . Free . SyncChannel s o $ FreeT . pure . Pure
