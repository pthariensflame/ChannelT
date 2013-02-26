{-# LANGUAGE GADTs, KindSignatures, RankNTypes, MultiParamTypeClasses, FunctionalDependencies, LiberalTypeSynonyms, FlexibleContexts #-}
module Control.Monad.Channel.Selector.Tee (TeeChannel,
                                           TeeChannelT,
                                           awaitLeft,
                                           awaitRight,
                                           yield,
                                           (>@+>),
                                           (<@+<),
                                           (>+@>),
                                           (<+@<),
                                           runTee,
                                           TeeSelector(..)) where
import Control.Monad.Channel
import Control.Monad.Channel.Selector.Empty
import Control.Monad.Channel.Selector.Pipe hiding (yield)
import qualified Control.Monad.Channel.Selector.Pipe as CMCSP (yield)
import Control.Monad.Trans.Free (FreeT(..), FreeF(..))
import Control.Applicative

data TeeSelector :: * -> * -> * -> * -> * -> * where
  AwaitLeftTee :: TeeSelector iL iR o iL ()
  AwaitRightTee :: TeeSelector iL iR o iR ()
  YieldTee :: TeeSelector iL iR o () o

type TeeChannel iL iR o a = Channel (TeeSelector iL iR o) a

type TeeChannelT iL iR o m a= ChannelT (TeeSelector iL iR o) m a

awaitLeft :: TeeChannel iL iR o iL
awaitLeft = syncOn AwaitLeftTee ()

awaitRight :: TeeChannel iL iR o iR
awaitRight = syncOn AwaitRightTee ()

yield :: o -> TeeChannel iL iR o ()
yield = syncOn YieldTee

(>@+>) :: (Applicative m, Monad m) => PipeChannelT iL mL m a -> TeeChannelT mL iR o m a -> TeeChannelT iL iR o m a
FreeT a >@+> FreeT b = FreeT $ do x <- a
                                  y <- b
                                  case (x, y) of
                                    (Pure v, _) -> return (Pure v)
                                    (_, Pure v) -> return (Pure v)
                                    (Free (SyncChannel AwaitPipe _ iL), _) -> runFreeT $ awaitLeft >>= \v -> iL v >@+> FreeT (return y)
                                    (_, Free (SyncChannel YieldTee oO iU)) -> runFreeT $ yield oO >> (FreeT (return x) >@+> iU ())
                                    (_, Free (SyncChannel AwaitRightTee _ iR)) -> runFreeT $ awaitRight >>= \v -> FreeT (return x) >@+> iR v
                                    (Free (SyncChannel YieldPipe oM iU), Free (SyncChannel AwaitLeftTee _ iM)) -> runFreeT $ iU () >@+> iM oM

(<@+<) :: (Applicative m, Monad m) => TeeChannelT mL iR o m a -> PipeChannelT iL mL m a -> TeeChannelT iL iR o m a
FreeT a <@+< FreeT b = FreeT $ do x <- a
                                  y <- b
                                  case (x, y) of
                                    (Pure v, _) -> return (Pure v)
                                    (_, Pure v) -> return (Pure v)
                                    (Free (SyncChannel YieldTee oO iU), _) -> runFreeT $ yield oO >> (iU () <@+< FreeT (return y))
                                    (Free (SyncChannel AwaitRightTee _ iR), _) -> runFreeT $ awaitRight >>= \v -> iR v <@+< FreeT (return y)
                                    (_, Free (SyncChannel AwaitPipe _ iL)) -> runFreeT $ awaitLeft >>= \v -> FreeT (return x) <@+< iL v
                                    (Free (SyncChannel AwaitLeftTee _ iM), Free (SyncChannel YieldPipe oM iU)) -> runFreeT $ iM oM <@+< iU ()

(>+@>) :: (Applicative m, Monad m) => PipeChannelT iR mR m a -> TeeChannelT iL mR o m a -> TeeChannelT iL iR o m a
FreeT a >+@> FreeT b = FreeT $ do x <- a
                                  y <- b
                                  case (x, y) of
                                    (Pure v, _) -> return (Pure v)
                                    (_, Pure v) -> return (Pure v)
                                    (Free (SyncChannel AwaitPipe _ iL), _) -> runFreeT $ awaitRight >>= \v -> iL v >+@> FreeT (return y)
                                    (_, Free (SyncChannel YieldTee oO iU)) -> runFreeT $ yield oO >> (FreeT (return x) >+@> iU ())
                                    (_, Free (SyncChannel AwaitLeftTee _ iL)) -> runFreeT $ awaitLeft >>= \v -> FreeT (return x) >+@> iL v
                                    (Free (SyncChannel YieldPipe oM iU), Free (SyncChannel AwaitRightTee _ iM)) -> runFreeT $ iU () >+@> iM oM

(<+@<) :: (Applicative m, Monad m) => TeeChannelT iL mR o m a -> PipeChannelT iR mR m a -> TeeChannelT iL iR o m a
FreeT a <+@< FreeT b = FreeT $ do x <- a
                                  y <- b
                                  case (x, y) of
                                    (Pure v, _) -> return (Pure v)
                                    (_, Pure v) -> return (Pure v)
                                    (Free (SyncChannel YieldTee oO iU), _) -> runFreeT $ yield oO >> (iU () <+@< FreeT (return y))
                                    (Free (SyncChannel AwaitLeftTee _ iL), _) -> runFreeT $ awaitLeft >>= \v -> iL v <+@< FreeT (return y)
                                    (_, Free (SyncChannel AwaitPipe _ iL)) -> runFreeT $ awaitRight >>= \v -> FreeT (return x) <+@< iL v
                                    (Free (SyncChannel AwaitRightTee _ iM), Free (SyncChannel YieldPipe oM iU)) -> runFreeT $ iM oM <+@< iU ()


runTee :: (Monad m) => TeeChannelT () () o m a -> EmptyChannelT m a
runTee (FreeT a) = FreeT $ a >>= \x -> case x of
  Pure v -> return (Pure v)
  Free (SyncChannel AwaitLeftTee _ iL) -> runFreeT . runTee $ iL ()
  Free (SyncChannel AwaitRightTee _ iR) -> runFreeT . runTee $ iR ()
  Free (SyncChannel YieldTee _ iU) -> runFreeT . runTee $ iU ()
