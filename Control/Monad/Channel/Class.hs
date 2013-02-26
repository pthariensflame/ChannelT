{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, KindSignatures, FlexibleInstances, RankNTypes, LiberalTypeSynonyms, FlexibleContexts #-}
module Control.Monad.Channel.Class (MonadChannel(..),
                                    Channel) where
import Control.Monad.Trans.Free (FreeT)
import Control.Monad.Trans.Channel (ChannelF)
import qualified Control.Monad.Trans.Channel as CMTC (syncOn)
import Control.Applicative (Applicative)

class (Monad m, Applicative m) => MonadChannel (sel :: * -> * -> *) (m :: * -> *) | m -> sel where
  syncOn :: sel i o -> o -> m i

instance (Monad m, Applicative m) => MonadChannel sel (FreeT (ChannelF sel) m) where
  syncOn = CMTC.syncOn

type Channel sel a = forall (m :: * -> *). (MonadChannel sel m) => m a
