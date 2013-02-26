{-# LANGUAGE GADTs, KindSignatures, RankNTypes, MultiParamTypeClasses, FunctionalDependencies, LiberalTypeSynonyms, FlexibleContexts #-}
module Control.Monad.Channel.Selector.Dimachine () where
import Control.Monad.Channel
import Control.Monad.Channel.Selector.Empty
import Control.Monad.Trans.Free (FreeT(..), FreeF(..))
import Control.Applicative
