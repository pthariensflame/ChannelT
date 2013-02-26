{-# LANGUAGE GADTs, KindSignatures, RankNTypes, MultiParamTypeClasses, FunctionalDependencies, LiberalTypeSynonyms, FlexibleContexts #-}
module Control.Monad.Channel.Selector.Comachine (ComachineChannel,
                                                 ComachineChannelT,
                                                 await,
                                                 yieldOn,
                                                 yield,
                                                 (>&>),
                                                 (<&<),
                                                 runComachine,
                                                 ComachineSelector(..)) where
import Prelude hiding (id)
import Control.Monad.Channel
import Control.Monad.Channel.Selector.Empty
import Control.Monad.Channel.Selector.Pipe hiding (await, yield)
import qualified Control.Monad.Channel.Selector.Pipe as CMCSP (await, yield)
import Control.Monad.Trans.Free (FreeT(..), FreeF(..))
import Control.Applicative
import Control.Category (Catogory(id))

data ComachineSelector :: * -> (* -> *) -> * -> * -> * where
  AwaitComachine :: ComachineSelector i kO i ()
  YieldOnComachine :: kO o -> ComachineSelector i kO () o

type ComachineChannel i kO a = Channel (ComachineSelector i kO) a

type ComachineChannelT i kO m a= ChannelT (ComachineSelector i kO) m a

await :: ComachineChannel i kO i
await = syncOn AwaitComachine ()

yieldOn :: kO o -> o -> ComachineChannel i kO ()
yieldOn = syncOn . YieldOnComachine

yield :: (Category c) => o -> ComachineChannel i (c o) ()
yield = yieldOn id o

(>&>) :: (Applicative m, Monad m) => PipeChannelT i q m a -> ComachineChannelT q kO m a -> ComachineChannelT i kO m a
FreeT a >&> FreeT b = FreeT $ do x <- a
                                 y <- b
                                 case (x, y) of
                                   (Pure v, _) -> return (Pure v)
                                   (_, Pure v) -> return (Pure v)
                                   (Free (SyncChannel AwaitPipe _ iI), _) -> runFreeT $ await >>= \v -> iI v >&> FreeT (return y)
                                   (_, Free (SyncChannel (YieldOnComachine k) oK iU)) -> runFreeT $ yieldOn k oK >> (FreeT (return x) >&> iU ())
                                   (Free (SyncChannel YieldPipe oQ iU), Free (SyncChannel AwaitComachine _ iQ)) -> runFreeT $ iU () >&> iQ oQ

(<&<) :: (Applicative m, Monad m) => ComachineChannelT q kO m a -> PipeChannelT i q m a -> ComachineChannelT i kO m a
FreeT a <&< FreeT b = FreeT $ do x <- a
                                 y <- b
                                 case (x, y) of
                                   (Pure v, _) -> return (Pure v)
                                   (_, Pure v) -> return (Pure v)
                                   (Free (SyncChannel (YieldOnComachine k) oK iU), _) -> runFreeT $ yieldOn k oK >> (iU () <&< FreeT (return x))
                                   (_, Free (SyncChannel AwaitPipe _ iI)) -> runFreeT $ await >>= \v -> FreeT (return x) <&< iI v
                                   (Free (SyncChannel AwaitComachine _ iQ), Free (SyncChannel YieldPipe oQ iU)) -> runFreeT $ iQ oQ <&< iU ()

runComachine :: (Monad m) => ComachineChannelT () kO m a -> m a
runComachine (FreeT a) = a >>= \x -> case x of
  Pure v -> return v
  Free (SyncChannel AwaitComachine _ iI) -> runComachine $ iI () 
  Free (SyncChannel (YieldOnComachine _) _ iU) -> runComachine $ iU ()
