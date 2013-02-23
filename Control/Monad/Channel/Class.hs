{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, KindSignatures, FlexibleInstances #-}
module Control.Monad.Channel.Class (MonadChannel(..)) where
import Control.Monad.Trans.Free (FreeT)
import qualified Control.Monad.Trans.Channel as CMTC (sync)

class (Monad m) => MonadChannel (sel :: * -> * -> *) (m :: * -> *) | m -> sel where
  sync :: sel i o -> o -> m i

instance (Monad m) => MonadChannel sel (FreeF (ChannelF sel) m) where
  sync = CMTC.sync
