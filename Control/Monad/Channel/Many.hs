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
import Control.Applicative

type Channels sels a = MultiChannel sels a

runChannels :: ChannelsT '[] Identity a -> a
runChannels = runIdentity . runChannelsT

type ChannelsT sels = MultiChannelT sels

runChannelsT :: (Functor m) => ChannelsT '[] m a -> m a
runChannelsT a = runChannelT $ runMulti a

type (:~>) = SingleSelector

sync :: o -> Channels ((i :~> o) ': sels) i
sync = syncWith SyncSingle

type family Append (xs :: [* -> * -> *]) (ys :: [* -> * -> *]) :: [* -> * -> *]
type instance Append '[] ys = ys
type instance Append (x ': xs) ys = x ': (Append xs ys)

infixl 7 >+<
(>+<) :: (Applicative m, Monad m) => ChannelsT ((SingleSelector x y) ': sels1) m a -> ChannelsT ((SingleSelector y x) ': sels2) m a -> ChannelsT (Append sels1 sels2) m a
FreeT a >+< FreeT b = FreeT $ do x <- a
                                 y <- b
                                 case (x, y) of
                                   (Pure v, _) -> return (Pure v)
                                   (_, Pure v) -> return (Pure v)
                                   (Free (SyncChannel (LiftSMulti s) o i), _) -> runFreeT $ liftS (syncOn s o) >>= \v -> i v >+< FreeT (return y)
                                   (_, Free (SyncChannel (LiftSMulti s) o i)) -> runFreeT $ liftS (liftS (syncOn s o)) >>= \v -> FreeT (return x) >+< i v
                                   (Free (SyncChannel (SyncWithMulti SyncSingle) oY iX), Free (SyncChannel (SyncWithMulti SyncSingle) oX iY)) -> runFreeT $ iX oX >+< iY oY
