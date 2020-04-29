{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RebindableSyntax           #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}

module Reader where

import           Prelude

import           Control.Exception    (ArithException (..), ArrayException,
                                       Exception)
import qualified Control.Exception    as E
import           Data.Typeable

import           Control.Monad.Reader


import           Control.Monad.Throw

data Config = Config
    { cField :: Int
    }

newtype App a = App ((ReaderT Config IO) a)
  deriving (Functor, Applicative, Monad, MonadReader Config, MonadIO)



-- computation :: Throws '[ ArithException ] IO Int
computation = do
  return 20
