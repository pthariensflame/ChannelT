{-# GADTs, KindSignatures, RankNTypes, LiberalTypeSynonyms #-}
module Control.Monad.Channel.Selector.Proxy (ProxyChannel,
                                             ProxyChannelT,
                                             runProxy,
                                             request,
                                             respond,
                                             (>->),
                                             (<-<),
                                             ProxySelector(..)) where
import Control.Monad.Channel
import Control.Monad.Channel.Selector.Empty
import Control.Monad.Trans.Free (FreeT(..), FreeF(..))
import Control.Applicative

data ProxySelector :: * -> * -> * -> * -> * -> * -> * where
  RequestProxy :: ProxySelector uO uI dI dO uI uO
  RespondProxy :: ProxySelector uO uI dI dO dI dO

type ProxyChannel uO uI dI dO = Channel (ProxySelector uO uI dI dO)

type ProxyChannelT uO uI dI dO = ChannelT (ProxySelector uO uI dI dO)

runProxy :: (Monad m) => ProxyChannelT uO () () dO m a -> EmptyChannelT m a
runProxy (FreeT a) = FreeT $ a >>= \x -> case x of
  (Pure a) -> return a
  (Free (SyncChannel RequestProxy _ i)) -> i ()
  (Free (SyncChannel RespondProxy _ i)) -> i ()

request :: uO -> ProxyChannel uO uI dI dO uI
request = syncOn RequestProxy

respond :: dO -> ProxyChannel uO uI dI dO dI
respond = syncOn RespondProxy

(>->) :: (Monad m) => ProxyChannelT uO uI mU mD m a -> ProxyChannelT mU mD dI dO m a -> ProxyChannelT uO uI dI dO m a
FreeT a >-> FreeT b = do x <- a
                         y <- b
                         case (x, y) of
                           (Pure _, _) -> return x
                           (_, Pure _) -> return y
                           (Free (SyncChannel RequestProxy oUO iUI), _) -> runFreeT $ request oUO >>= \v -> iUI v >-> FreeT (return y)
                           (_, Free (SyncChannel RespondProxy oDO iDI)) -> runFreeT $ respond oDO >>= \v -> FreeT (return x) >-> iDI v
                           (Free (SyncChannel RespondProxy oMD iMU), Free (SyncChannel RequestProxy oMU, iMD)) -> runFreeT $ iMU oMU >-> iMD oMD

(<-<) :: (Monad m) => ProxyChannelT mU mD dI dO m a -> ProxyChannelT uO uI mU mD m a -> ProxyChannelT uO uI dI dO m a
FreeT a <-< FreeT b = do x <- a
                         y <- b
                         case (x, y) of
                           (Pure _, _) -> return x
                           (_, Pure _) -> return y
                           (Free (SyncChannel RespondProxy oDO iDI), _) -> runFreeT $ respond oDO >>= \v -> iDI v <-< FreeT (return y)
                           (_, Free (SyncChannel RequestProxy oUO iUI)) -> runFreeT $ request oUO >>= \v -> FreeT (return x) <-< iUI v
                           (Free (SyncChannel RequestProxy oMU iMD), Free (SyncChannel RespondProxy oMD iMU)) -> runFreeT $ iMD oMD <-< iMU oMU
