{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, KindSignatures, FlexibleInstances #-}
module Control.Monad.Channel.Class (MonadChannel(..)) where
import Control.Monad.Trans.Free (FreeT)
import Control.Monad.Trans.Channel (ChannelF)
import qualified Control.Monad.Trans.Channel as CMTC (sync)
import Control.Applicative (Applicative)

class (Monad m, Applicative m) => MonadChannel (sel :: * -> * -> *) (m :: * -> *) | m -> sel where
  sync :: sel i o -> o -> m i

instance (Monad m, Applicative m) => MonadChannel sel (FreeT (ChannelF sel) m) where
  sync = CMTC.sync
