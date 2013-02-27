{-# LANGUAGE GADTs, KindSignatures #-}
module Data.KUnit (KUnit(..)) where

data KUnit :: * -> * where
  KUnit :: KUnit ()
