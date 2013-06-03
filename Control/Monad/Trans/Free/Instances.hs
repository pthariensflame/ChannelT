module Control.Monad.Trans.Free.Instances () where
import Control.Monad.Trans
import Control.Monad.Identity
import Control.Monad.Base
import Control.Monad.Trans.Free (FreeT(..), FreeF(..))

instance (MonadBase b m, Functor f) => MonadBase b (FreeT f m) where
  liftBase = liftBaseDefault

instance 
