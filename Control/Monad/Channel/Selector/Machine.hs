{-# LANGUAGE GADTs, KindSignatures, RankNTypes, MultiParamTypeClasses, FunctionalDependencies, LiberalTypeSynonyms, FlexibleContexts #-}
module Control.Monad.Channel.Selector.Machine (MachineChannel,
                                               MachineChannelT,
                                               awaitOn,
                                               await,
                                               yield,
                                               (>@>),
                                               (<@<),
                                               runMachine,
                                               MachineSelector(..)) where
import Prelude hiding (id)
import Control.Monad.Channel
import Control.Monad.Channel.Selector.Empty
import Control.Monad.Channel.Selector.Pipe hiding (await, yield)
import qualified Control.Monad.Channel.Selector.Pipe as CMCSP (await, yield)
import Control.Monad.Trans.Free (FreeT(..), FreeF(..))
import Control.Applicative
import Control.Category (Category(id))
import Data.KUnit

data MachineSelector :: (* -> *) -> * -> * -> * -> * where
  AwaitOnMachine :: kI i -> MachineSelector kI o i ()
  YieldMachine :: MachineSelector kI o () o

type MachineChannel kI o a = Channel (MachineSelector kI o) a

type MachineChannelT kI o = ChannelT (MachineSelector kI o)

awaitOn :: kI i -> MachineChannel kI o i
awaitOn k = syncOn (AwaitOnMachine k) ()

await :: (Category c) => MachineChannel (c i) o i
await = awaitOn id

yield :: o -> MachineChannel kI o ()
yield = syncOn YieldMachine

infixl 7 >@>
(>@>) :: (Applicative m, Monad m) => MachineChannelT kI q m a -> PipeChannelT q o m a -> MachineChannelT kI o m a
FreeT a >@> FreeT b = FreeT $ do x <- a
                                 y <- b
                                 case (x, y) of
                                   (Pure v, _) -> return (Pure v)
                                   (_, Pure v) -> return (Pure v)
                                   (Free (SyncChannel (AwaitOnMachine k) _ iK), _) -> runFreeT $ awaitOn k >>= \v -> iK v >@> FreeT (return y)
                                   (_, Free (SyncChannel YieldPipe oO iU)) -> runFreeT $ yield oO >> (FreeT (return x) >@> iU ())
                                   (Free (SyncChannel YieldMachine oQ iU), Free (SyncChannel AwaitPipe _ iQ)) -> runFreeT $ iU () >@> iQ oQ

infixl 7 <@<
(<@<) :: (Applicative m, Monad m) => PipeChannelT q o m a -> MachineChannelT kI q m a -> MachineChannelT kI o m a
FreeT a <@< FreeT b = FreeT $ do x <- a
                                 y <- b
                                 case (x, y) of
                                   (Pure v, _) -> return (Pure v)
                                   (_, Pure v) -> return (Pure v)
                                   (Free (SyncChannel YieldPipe oO iU), _) -> runFreeT $ yield oO >> (iU () <@< FreeT (return y))
                                   (_, Free (SyncChannel (AwaitOnMachine k) _ iK)) -> runFreeT $ awaitOn k >>= \v -> FreeT (return x) <@< iK v
                                   (Free (SyncChannel AwaitPipe _ iQ), Free (SyncChannel YieldMachine oQ iU)) -> runFreeT $ iQ oQ <@< iU ()

runMachine :: (Monad m) => MachineChannelT KUnit o m a -> EmptyChannelT m a
runMachine (FreeT a) = FreeT $ a >>= \x -> case x of
  Pure v -> return (Pure v)
  Free (SyncChannel (AwaitOnMachine KUnit) _ iK) -> runFreeT . runMachine $ iK ()
  Free (SyncChannel YieldMachine _ iU) -> runFreeT . runMachine $ iU ()
