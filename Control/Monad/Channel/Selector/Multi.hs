{-# LANGUAGE GADTs, KindSignatures, RankNTypes, MultiParamTypeClasses, FunctionalDependencies, LiberalTypeSynonyms, FlexibleContexts, DataKinds, TypeOperators #-}
module Control.Monad.Channel.Selector.Multi (MultiChannel,
                                             MultiChannelT,
                                             runMulti,
                                             syncWith,
                                             liftS,
                                             MultiSelector(..)) where
import Control.Monad.Channel
import Control.Monad.Channel.Selector.Empty
import Control.Monad.Channel.Selector.Single
import Control.Monad.Trans.Free (FreeT(..), FreeF(..))
import Control.Applicative

data MultiSelector :: [* -> * -> *] -> * -> * -> * where
  SyncWithMulti :: sel i o -> MultiSelector (sel ': sels) i o
  LiftSMulti :: MultiSelector sels i o -> MultiSelector (sel ': sels) i o

type MultiChannel sels a = Channel (MultiSelector sels) a

type MultiChannelT sels = ChannelT (MultiSelector sels)

syncWith :: sel i o -> o -> MultiChannel (sel ': sels) i
syncWith = syncOn . SyncWithMulti

liftS :: (Functor m) => MultiChannelT sels m a -> MultiChannelT (sel ': sels) m a
liftS (FreeT a) = FreeT $ fmap (\x -> case x of
                                   Pure v -> Pure v
                                   Free (SyncChannel s o i) -> Free (SyncChannel (LiftSMulti s) o (liftS . i))) a

runMulti :: (Functor m) => MultiChannelT '[] m a -> EmptyChannelT m a
runMulti (FreeT a) = FreeT $ fmap (\(Pure v) -> Pure v) a
