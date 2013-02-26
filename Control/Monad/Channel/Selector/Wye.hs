{-# LANGUAGE GADTs, KindSignatures, RankNTypes, MultiParamTypeClasses, FunctionalDependencies, LiberalTypeSynonyms, FlexibleContexts #-}
module Control.Monad.Channel.Selector.Wye (WyeChannel,
                                           WyeChannelT,
                                           awaitLeft,
                                           awaitRight,
                                           awaitWye,
                                           yield,
                                           (>&+>),
                                           (<&+<),
                                           (>+&>),
                                           (<+&<),
                                           runWye,
                                           WyeSelector(..)) where
import Control.Monad.Channel
import Control.Monad.Channel.Selector.Empty
import Control.Monad.Channel.Selector.Pipe hiding (yield)
import qualified Control.Monad.Channel.Selector.Pipe as CMCSP (yield)
import Control.Monad.Trans.Free (FreeT(..), FreeF(..))
import Control.Applicative

data WyeSelector :: * -> * -> * -> * -> * -> * where
  AwaitLeftWye :: WyeSelector iL iR o iL ()
  AwaitRightWye :: WyeSelector iL iR o iR ()
  AwaitWyeWye :: WyeSelector iL iR o (Either iL iR) ()
  YieldWye :: WyeSelector iL iR o () o

type WyeChannel iL iR o a = Channel (WyeSelector iL iR o) a

type WyeChannelT iL iR o m a = ChannelT (WyeSelector iL iR o) m a

awaitLeft :: WyeChannel iL iR o iL
awaitLeft = syncOn AwaitLeftWye ()

awaitRight :: WyeChannel iL iR o iR
awaitRight = syncOn AwaitRightWye ()

awaitWye :: WyeChannel iL iR o (Either iL iR)
awaitWye = syncOn AwaitWyeWye ()

yield :: o -> WyeChannel iL iR o ()
yield = syncOn YieldWye

(>&+>) :: (Applicative m, Monad m) => PipeChannelT iL mL m a -> WyeChannelT mL iR o m a -> WyeChannelT iL iR o m a
FreeT a >&+> FreeT b = FreeT $ do x <- a
                                  y <- b
                                  case (x, y) of
                                    (Pure v, _) -> return (Pure v)
                                    (_, Pure v) -> return (Pure v)
                                    (Free (SyncChannel AwaitPipe _ iL), _) -> runFreeT $ awaitLeft >>= \v -> iL v >&+> FreeT (return y)
                                    (_, Free (SyncChannel YieldWye oO iU)) -> runFreeT $ yield oO >> (FreeT (return x) >&+> iU ())
                                    (_, Free (SyncChannel AwaitRightWye _ iR)) -> runFreeT $ awaitRight >>= \v -> FreeT (return x) >&+> iR v
                                    (Free (SyncChannel YieldPipe oM iU), Free (SyncChannel AwaitLeftWye _ iM)) -> runFreeT $ iU () >&+> iM oM
                                    (Free (SyncChannel YieldPipe oM iU), Free (SyncChannel AwaitWyeWye _ iE)) -> runFreeT $ iU () >&+> iE (Left oM)

(<&+<) :: (Applicative m, Monad m) => WyeChannelT mL iR o m a -> PipeChannelT iL mL m a -> WyeChannelT iL iR o m a
FreeT a <&+< FreeT b = FreeT $ do x <- a
                                  y <- b
                                  case (x, y) of
                                    (Pure v, _) -> return (Pure v)
                                    (_, Pure v) -> return (Pure v)
                                    (Free (SyncChannel YieldWye oO iU), _) -> runFreeT $ yield oO >> (iU () <&+< FreeT (return y))
                                    (Free (SyncChannel AwaitRightWye _ iR), _) -> runFreeT $ awaitRight >>= \v -> iR v <&+< FreeT (return y)
                                    (_, Free (SyncChannel AwaitPipe _ iL)) -> runFreeT $ awaitLeft >>= \v -> FreeT (return x) <&+< iL v
                                    (Free (SyncChannel AwaitLeftWye _ iM), Free (SyncChannel YieldPipe oM iU)) -> runFreeT $ iM oM <&+< iU ()
                                    (Free (SyncChannel AwaitWyeWye _ iE), Free (SyncChannel YieldPipe oM iU)) -> runFreeT $ iE (Left oM) <&+< iU ()

(>+&>) :: (Applicative m, Monad m) => PipeChannelT iR mR m a -> WyeChannelT iL mR o m a -> WyeChannelT iL iR o m a
FreeT a >+&> FreeT b = FreeT $ do x <- a
                                  y <- b
                                  case (x, y) of
                                    (Pure v, _) -> return (Pure v)
                                    (_, Pure v) -> return (Pure v)
                                    (Free (SyncChannel AwaitPipe _ iL), _) -> runFreeT $ awaitRight >>= \v -> iL v >+&> FreeT (return y)
                                    (_, Free (SyncChannel YieldWye oO iU)) -> runFreeT $ yield oO >> (FreeT (return x) >+&> iU ())
                                    (_, Free (SyncChannel AwaitLeftWye _ iL)) -> runFreeT $ awaitLeft >>= \v -> FreeT (return x) >+&> iL v
                                    (Free (SyncChannel YieldPipe oM iU), Free (SyncChannel AwaitRightWye _ iM)) -> runFreeT $ iU () >+&> iM oM
                                    (Free (SyncChannel YieldPipe oM iU), Free (SyncChannel AwaitWyeWye _ iE)) -> runFreeT $ iU () >+&> iE (Right oM)

(<+&<) :: (Applicative m, Monad m) => WyeChannelT iL mR o m a -> PipeChannelT iR mR m a -> WyeChannelT iL iR o m a
FreeT a <+&< FreeT b = FreeT $ do x <- a
                                  y <- b
                                  case (x, y) of
                                    (Pure v, _) -> return (Pure v)
                                    (_, Pure v) -> return (Pure v)
                                    (Free (SyncChannel YieldWye oO iU), _) -> runFreeT $ yield oO >> (iU () <+&< FreeT (return y))
                                    (Free (SyncChannel AwaitLeftWye _ iL), _) -> runFreeT $ awaitLeft >>= \v -> iL v <+&< FreeT (return y)
                                    (_, Free (SyncChannel AwaitPipe _ iL)) -> runFreeT $ awaitRight >>= \v -> FreeT (return x) <+&< iL v
                                    (Free (SyncChannel AwaitRightWye _ iM), Free (SyncChannel YieldPipe oM iU)) -> runFreeT $ iM oM <+&< iU ()
                                    (Free (SyncChannel AwaitWyeWye _ iE), Free (SyncChannel YieldPipe oM iU)) -> runFreeT $ iE (Right oM) <+&< iU ()


runWye :: (Applicative m, Monad m) => WyeChannelT () () o m a -> EmptyChannelT m a
runWye (FreeT a) = FreeT $ a >>= \x -> case x of
  Pure v -> return (Pure v)
  Free (SyncChannel AwaitLeftWye _ iL) -> runFreeT $ iL () >>= runWye
  Free (SyncChannel AwaitRightWye _ iR) -> runFreeT $ iR () >>= runWye
  Free (SyncChannel AwaitWyeWye _ iW) -> runFreeT $ iW (Left ()) >>= runWye
  Free (SyncChannel YieldWye _ iU) -> runFreeT $ iU () >>= runWye
