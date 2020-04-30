{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RebindableSyntax           #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}

module Reader where

import           Control.Exception    (ArithException (..),
                                       AssertionFailed (..))
import           Control.Monad.Reader
import           Prelude

import           Control.Monad.Throw

newtype App a = App ((ReaderT Config IO) a)
  deriving (Functor, Applicative, Monad, MonadReader Config, MonadIO)

data Config = Config
    { cField :: Int
    }

deriving instance MonadThrow Throws App

runApp :: App a -> Config -> IO a
runApp (App r) c = runReaderT r c

-- computation :: Throws '[ AssertionFailed , ArithException ] App Int
computation = do
  n :: Int <- safe $ asks cField
  if n == 0
     then throw $ AssertionFailed "Zeroes are bad"
     else do
       safe $ liftIO $ putStrLn "Nebula"
       throws @'[ ArithException ] $ liftIO $ putStrLn $ show $ n + 1
       result $ n + 2
    where
      (>>=) = (>!=)  @Throws
      (>>) = (>!)
      ifThenElse = cond

hArith :: Handler ArithException App Int
hArith = Handler $ \_ -> do
  n :: Int <- asks cField
  return n

-- Removing `ignore` produces a compile time error:
-- • Not handled exception: AssertionFailed
-- • In the expression: ...
app :: App Int
app = catches (ignore @'[AssertionFailed] computation) (hArith :&&: Nil)

foul :: IO Int
foul = runApp app (Config 0)

holy :: IO Int
holy = runApp app (Config 1)


