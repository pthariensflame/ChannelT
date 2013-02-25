{-# LANGUAGE KindSignatures, RankNTypes, LiberalTypeSynonyms, EmptyDataDecls #-}
module Control.Monad.Channel.Selector.Empty (EmptyChannel,
                                             runChannel,
                                             EmptyChannelT,
                                             runChannelT,
                                             EmptySelector(..)) where
import Control.Monad.Trans.Channel
import Control.Monad.Identity
import Control.Monad.Trans.Free (FreeT(..), FreeF(..))

type EmptyChannel = forall (sel :: * -> * -> *) (m :: * -> *). MonadChannel sel m => m

runChannel :: EmptyChannelT Identity a -> a
runChannel = runIdentity . runChannelT

type EmptyChannelT = forall (sel :: * -> * -> *). ChannelT sel

runChannelT :: (Functor m) => EmptyChannelT m a -> m a
runChannelT (FreeT a) = fmap (\(Pure x) -> x) a

data EmptySelector (i :: *) (o :: *)
