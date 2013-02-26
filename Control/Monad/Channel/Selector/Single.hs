{-# LANGUAGE GADTs, KindSignatures, RankNTypes, LiberalTypeSynonyms, FlexibleContexts #-}
module Control.Monad.Channel.Selector.Single (SingleChannel,
                                              SingleChannelT,
                                              sync,
                                              (>-<),
                                              SingleSelector(..)) where
import Control.Monad.Channel
import Control.Monad.Channel.Selector.Empty
import Control.Monad.Trans.Free (FreeT(..), FreeF(..))
import Control.Applicative

type SingleChannel i o a = Channel (SingleSelector i o) a

type SingleChannelT i o m a = ChannelT (SingleSelector i o) m a

data SingleSelector :: * -> * -> * -> * -> * where
  Single :: SingleSelector i o i o

sync :: o -> SingleChannel i o i
sync = syncOn Single

(>-<) :: (Monad m) => ChannelT (SingleSelector x y) m a -> ChannelT (SingleSelector y x) m a -> EmptyChannelT m a
FreeT a >-< FreeT b = FreeT $ do x <- a
                                 y <- b
                                 case (x, y) of
                                   (Pure v, _) -> return (Pure v)
                                   (_, Pure v) -> return (Pure v)
                                   (Free (SyncChannel Single oY iX), Free (SyncChannel Single oX iY)) -> runFreeT $ iX oX >-< iY oY