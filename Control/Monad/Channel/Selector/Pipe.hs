{-# LANGUAGE GADTs, KindSignatures, RankNTypes, LiberalTypeSynonyms, FlexibleContexts #-}
module Control.Monad.Channel.Selector.Pipe (PipeChannel,
                                            PipeChannelT,
                                            runPipe,
                                            await,
                                            yield,
                                            (>+>),
                                            (<+<),
                                            PipeSelector(..)) where
import Control.Monad.Channel
import Control.Monad.Channel.Selector.Empty
import Control.Monad.Trans.Free (FreeT(..), FreeF(..))
import Control.Applicative

type PipeChannel i o a = Channel (PipeSelector i o) a

type PipeChannelT i o m a = ChannelT (PipeSelector i o) m a

data PipeSelector :: * -> * -> * -> * -> * where
  AwaitPipe :: PipeSelector i o i ()
  YieldPipe :: PipeSelector i o () o

runPipe :: (Monad m) => PipeChannelT () o m a -> EmptyChannelT m a
runPipe (FreeT a) = FreeT $ a >>= \x -> case x of
  Pure v -> return (Pure v)
  Free (SyncChannel AwaitPipe _ i) -> runFreeT . runPipe $ i ()
  Free (SyncChannel YieldPipe _ i) -> runFreeT . runPipe $ i ()

await :: PipeChannel i o i
await = syncOn AwaitPipe ()

yield :: o -> PipeChannel i o ()
yield = syncOn YieldPipe

(>+>) :: (Applicative m, Monad m) => PipeChannelT x y m a -> PipeChannelT y z m a -> PipeChannelT x z m a
FreeT a >+> FreeT b = FreeT $ do x <- a
                                 y <- b
                                 case (x, y) of
                                   (Pure v, _) -> return (Pure v)
                                   (_, Pure v) -> return (Pure v)
                                   (Free (SyncChannel AwaitPipe _ iX), _) -> runFreeT $ await >>= \v -> iX v >+> FreeT (return y)
                                   (_, Free (SyncChannel YieldPipe oZ iU)) -> runFreeT $ yield oZ >> (FreeT (return x) >+> iU ())
                                   (Free (SyncChannel YieldPipe oY iU), Free (SyncChannel AwaitPipe _ iY)) -> runFreeT $ iU () >+> iY oY

(<+<) :: (Applicative m, Monad m) => PipeChannelT y z m a -> PipeChannelT x y m a -> PipeChannelT x z m a
FreeT a <+< FreeT b = FreeT $ do x <- a
                                 y <- b
                                 case (x, y) of
                                   (Pure v, _) -> return (Pure v)
                                   (_, Pure v) -> return (Pure v)
                                   (Free (SyncChannel YieldPipe oZ iU), _) -> runFreeT $ yield oZ >> (iU () <+< FreeT (return y))
                                   (_, Free (SyncChannel AwaitPipe _ iX)) -> runFreeT $ await >>= \v -> FreeT (return x) <+< iX v
                                   (Free (SyncChannel AwaitPipe _ iY), Free (SyncChannel YieldPipe oY iU)) -> runFreeT $ iY oY <+< iU ()
