{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RebindableSyntax           #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}

module Control.Monad.Throw.Tests (tests) where

import           Test.Framework         (Test, testGroup)

import           Control.Exception      hiding (Handler, catch, catches, throw)
import qualified Control.Exception      as E
import           Control.Monad.IO.Class
import           Data.Typeable
import           GHC.TypeLits
import           Prelude

import           Control.Monad.Throw

tests :: Test
tests = undefined

data ExampleException = Example1
    | Example2
    deriving (Show, Typeable)

data LovelyException = Heartbroken
    deriving (Show, Typeable)

instance Exception ExampleException
instance Exception LovelyException

test1 :: Throws '[ ArithException, LovelyException, ExampleException ] IO ()
test1
  = throws @'[ ArithException ] (putStrLn "hello")
  >! (throw Heartbroken)
  >! (throw Example1)
  >! (throw Example2)


test2 :: IO ()
test2 = catches test1 (hArith :&&: hLovely :&&: hExample :&&: Nil)

hArith :: Handler ArithException ()
hArith = Handler (putStrLn . show)

hArray :: Handler ArrayException ()
hArray = Handler (putStrLn . show)

hLovely :: Handler LovelyException ()
hLovely = Handler (putStrLn . show)

hExample :: Handler ExampleException ()
hExample = Handler (putStrLn . show)

type ManyExceptions = '[ ArithException, ArrayException ]

test3 :: Throws (ManyExceptions :++: '[ LovelyException, ExampleException ]) IO ()
test3 = do
  throws @ManyExceptions action
  throw Heartbroken
  throw Example1
  throw Example2
  where
    (>>) = (>!)

action :: IO ()
action = do
  putStrLn "I'm quite a naughty function"
  putStrLn "Can throw so many things!"


test4 = ignore' @(ManyExceptions :++: '[ErrorCall]) test3
