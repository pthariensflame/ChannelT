{-# LANGUAGE KindSignatures, EmptyDataDecls, ExistentialQuantification #-}
module Control.Monad.Trans.Channel (Channel,
                                    runChannel,
                                    ChannelT,
                                    runChannelT,
                                    sync,
                                    EmptySel,
                                    -- * Internals
                                    ChannelF(..)) where
import Control.Monad.Trans
import Control.Monad.Identity
import Control.Monad.Base
import Control.Monad.Trans.Free (FreeT(..), FreeF(..))
import Control.Applicative (Applicative(..))

type Channel sel = ChannelT sel Identity

runChannel :: Channel sel a -> a
runChannel = runIdentity . runChannelT

type ChannelT sel = FreeT (ChannelF sel)

runChannelT :: (Functor m) => ChannelT sel m a -> m a
runChannelT (FreeT a) = fmap (\(Pure x) -> x) a
                           

data ChannelF sel x = forall (i :: *) (o :: *). SyncChannel (sel i o) o (i -> x)

instance Functor (ChannelF sel) where
  fmap f (SyncChannel s o i) = SyncChannel s o (f . i)

sync :: (Applicative m) => sel i o -> o -> ChannelT sel m i
sync s o = FreeT . pure . Free . SyncChannel s o $ FreeT . pure . Pure

data EmptySel (i :: *) (o :: *)
