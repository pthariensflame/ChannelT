{-# LANGUAGE GADTs, KindSignatures, RankNTypes, MultiParamTypeClasses, FunctionalDependencies, LiberalTypeSynonyms, FlexibleContexts, DataKinds, TypeOperators, TypeFamilies #-}
module Control.Monad.Channel.Many (module Control.Monad.Channel,
                                   Channels,
                                   runChannels,
                                   ChannelsT,
                                   runChannelsT,
                                   (:~>),
                                   sync,
                                   liftS,
                                   (>+<)) where
import Control.Monad.Channel
import Control.Monad.Channel.Selector.Multi
import Control.Monad.Channel.Selector.Single (SingleSelector(..))
import Control.Monad.Channel.Selector.Tee (TeeSelector(..))
import Control.Monad.Channel.Selector.Wye (WyeSelector(..))
import Control.Monad.Identity
import Control.Monad.Trans.Free (FreeT(..), FreeF(..))

type family Interleave (xs :: [* -> * -> *]) (ys :: [* -> * -> *]) :: [* -> * -> *]
type instance Interleave '[] '[] = '[]
type instance Interleave (x ': xs) '[] = x ': xs
type instance Interleave '[] (y ': ys) = y ': ys
type instance Interleave (x ': xs) (y ': ys) = x ': (y ': (Interleave xs ys))

type Channels sels a = MultiChannel sels a

runChannels :: ChannelsT '[] Identity a -> a
runChannels = runIdentity . runChannelsT

type ChannelsT sels = MultiChannelT sels

runChannelsT :: (Functor m) => ChannelsT '[] m a -> m a
runChannelsT a = runChannelT $ runMulti a

type (:~>) = SingleSelector

sync :: o -> Channels ((i :~> o) ': sels) i
sync = syncWith SyncSingle

infixl 7 >+<
(>+<) :: (Monad m) => ChannelsT ((SingleSelector x y) ': sels1) m a -> ChannelsT ((SingleSelector y x) ': sels2) m a -> ChannelsT (Interleave sels1 sels2) m a 
FreeT a >+< FreeT b = FreeT $ do x <- a
                                 y <- b
                                 case (x, y) of
                                   (Pure v, _) -> Pure v
                                   (_, Pure v) -> Pure v
                                   (Free (SyncChannel (LiftSMulti s) o i), _) -> runFreeT $ liftS (syncOn s o) >>= \v -> i v >+< FreeT (return y)
                                   (_, Free (SyncChannel (LiftSMulti s) o i)) -> runFreeT $ liftS (liftS (syncOn s o)) >>= \v -> FreeT (return x) >+< i v
                                   (Free (SyncChannel (SyncWithMulti SyncSingle) oY iX), Free (SyncChannel (SyncWithMulti SyncSingle) oX iY)) -> runFreeT $ iX oX >+< iY oY
