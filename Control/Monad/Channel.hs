{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, KindSignatures, EmptyDataDecls #-}
module Control.Monad.Channel (Channel,
                              runChannel,
                              ChannelT,
                              runChannelT,
                              MonadChannel(..),
                              module Control.Monad.Trans,
                              module Control.Monad.Identity,
                              module Control.Monad.Base) where
import Control.Monad.Trans.Channel hiding (sync)
import Control.Monad.Channel.Class
import Control.Monad.Trans
import Control.Monad.Identity
import Control.Monad.Base
