{-# LANGUAGE GADTs, KindSignatures, RankNTypes, MultiParamTypeClasses, FunctionalDependencies, LiberalTypeSynonyms, FlexibleContexts #-}
module Control.Monad.Channel.Selector.Dimachine (DimachineChannel,
                                                 DimachineChannelT,
                                                 awaitOn,
                                                 await,
                                                 yieldOn,
                                                 yield,
                                                 runDimachine,
                                                 DimachineSelector(..)) where
import Prelude hiding (id)
import Control.Monad.Channel
import Control.Monad.Channel.Selector.Empty
import Control.Monad.Trans.Free (FreeT(..), FreeF(..))
import Control.Applicative
import Control.Category (Category(id))
import Data.KUnit

data DimachineSelector :: (* -> *) -> (* -> *) -> * -> * -> * where
  AwaitOnDimachine :: kI i -> DimachineSelector kI kO i ()
  YieldOnDimachine :: kO o -> DimachineSelector kI kO () o

type DimachineChannel kI kO a = Channel (DimachineSelector kI kO) a

type DimachineChannelT kI kO m a = ChannelT (DimachineSelector kI kO) m a

awaitOn :: kI i -> DimachineChannel kI kO i
awaitOn k = syncOn (AwaitOnDimachine k) ()

await :: (Category c) => DimachineChannel (c i) kO i
await = awaitOn id

yieldOn :: kO o -> o -> DimachineChannel kI kO ()
yieldOn k = syncOn (YieldOnDimachine k)

yield :: (Category c) => o -> DimachineChannel kI (c o) ()
yield = yieldOn id

runDimachine :: (Monad m) => DimachineChannelT KUnit kO m a -> EmptyChannelT m a
runDimachine (FreeT a) = FreeT $ a >>= \x -> case x of
  Pure v -> return (Pure v)
  Free (SyncChannel (AwaitOnDimachine KUnit) _ iK) -> runFreeT . runDimachine $ iK ()
  Free (SyncChannel (YieldOnDimachine _) _ iU) -> runFreeT . runDimachine $ iU ()
