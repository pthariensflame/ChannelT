{-# LANGUAGE KindSignatures, RankNTypes, LiberalTypeSynonyms, EmptyDataDecls #-}
module Control.Monad.Channel.Selector.Empty (EmptyChannel,
                                             runChannel,
                                             EmptyChannelT,
                                             runChannelT,
                                             EmptySelector(..)) where
import Control.Monad.Trans.Channel hiding (syncOn)
import Control.Monad.Channel.Class
import Control.Monad.Identity
import Control.Monad.Trans.Free (FreeT(..), FreeF(..))

type EmptyChannel a = forall (sel :: * -> * -> *) (m :: * -> *). MonadChannel sel m => m a

runChannel :: EmptyChannelT Identity a -> a
runChannel = runIdentity . runChannelT

type EmptyChannelT m a = forall (sel :: * -> * -> *). ChannelT sel m a

runChannelT :: (Functor m) => EmptyChannelT m a -> m a
runChannelT (FreeT a) = fmap (\(Pure x) -> x) a

data EmptySelector (i :: *) (o :: *)
