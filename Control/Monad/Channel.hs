{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, KindSignatures, EmptyDataDecls, GADTs, ExistentialQuantification, FlexibleInstances, RankNTypes, LiberalTypeSynonyms #-}
module Control.Monad.Channel (Channel,
                              EmptyChannel,
                              runChannel,
                              ChannelT,
                              EmptyChannelT,
                              runChannelT,
                              MonadChannel(..),
                              module Control.Monad.Trans,
                              module Control.Monad.Identity,
                              module Control.Monad.Base,
                              -- * Internals
                              ChannelF(..)) where
import Control.Monad.Trans.Channel hiding (syncOn)
import Control.Monad.Channel.Class
import Control.Monad.Channel.Selector.Empty
import Control.Monad.Trans
import Control.Monad.Identity
import Control.Monad.Base
