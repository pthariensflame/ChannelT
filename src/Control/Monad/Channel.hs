{-# LANGUAGE Safe #-}
module Control.Monad.Channel (
  Channel,
  ChannelT,
  MonadChannel(..),
) where
import Control.Monad.Channel.Internal
