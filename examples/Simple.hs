{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RebindableSyntax           #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}

module Simple where

import           Prelude

import           Control.Exception   (ArithException (..), ArrayException,
                                      Exception)
import qualified Control.Exception   as E
import           Data.Typeable

import           Control.Monad.Throw

-- Our own exceptions
data ExampleException = Example1
    | Example2
    deriving (Show, Typeable)

data LovelyException = Heartbroken
    deriving (Show, Typeable)

instance Exception ExampleException
instance Exception LovelyException

-- Handlers
hArith :: Handler ArithException ()
hArith = Handler (putStrLn . show)

hArray :: Handler ArrayException ()
hArray = Handler (putStrLn . show)

hLovely :: Handler LovelyException ()
hLovely = Handler (putStrLn . show)

hExample :: Handler ExampleException ()
hExample = Handler (putStrLn . show)

-- Type synonyms, so you shouldn't write the same exceptions all the time
type SillyExceptions = '[ ArithException, ArrayException ]

-- A type can be inferred...
action :: Throws (SillyExceptions :++: '[ LovelyException, ExampleException ]) IO ()
action = do
  throws @SillyExceptions externalAction
  throw Heartbroken
  throw Example1
  throw Example2
  where
    (>>) = (>!) -- @Throws | ...Type application would be needed to disambiguate though.


-- An external action, we don't know much about.
-- But our assumptions were expressed with `throws`.
externalAction :: IO ()
externalAction = do
  putStrLn "I'm quite a naughty function"
  putStrLn "Shouldn't you be scared using me?"
  E.throw Denormal


-- It is a type error to not handle every possible exception.
-- But we can use `ignore` to drop handling of exceptions
-- we do not care about.
safety :: IO ()
safety = catches
          (ignore @'[ArrayException] action)
          (hArith :&&: hLovely :&&: hExample :&&: Nil)


