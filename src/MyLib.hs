{-# LANGUAGE DataKinds, FlexibleInstances, MultiParamTypeClasses                  #-}
{-# LANGUAGE GADTs, TypeApplications                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances, RebindableSyntax       #-}

module MyLib () where

import           Control.Exception      hiding (catches, throw, Handler)
import qualified Control.Exception      as E
import           Control.Monad.IO.Class
import           Data.Kind              (Constraint, Type)
import           Data.Typeable
import           GHC.TypeLits
import           Prelude

infixl 4 :\\:
type family (:\\:) (as :: [k]) (bs :: [k]) :: [k] where
  as :\\: '[] = as
  as :\\: (b ': bs) = Remove b as :\\: bs

type family Remove (a :: k) (as :: [k]) :: [k] where
  Remove a (a ': as) = as
  Remove a (b ': bs) = b ': Remove a bs

type family Member (a :: k) (as :: [k]) :: Bool where
  Member a '[] = 'False
  Member a (a ': _) = 'True
  Member a (_ ': as) = Member a as

type family If (p :: Bool) (a :: k) (b :: k) :: k where
  If 'True a _ = a
  If 'False _ b = b

infixr 5 :++:
type family (:++:) (a :: [k]) (b :: [k]) :: [k] where
  '[] :++: bs = bs
  (a:as) :++: bs = If (Member a bs) (as :++: bs) (a ': (as :++: bs))

class MonadThrow (t :: [Type] -> (Type -> Type) -> (Type -> Type)) where
  inject :: forall (es :: [Type]) m a. m a -> t es m a
  strip :: t es m a -> m a 
  stripIO :: t es m a -> IO a

  (>!=) :: (Monad m) => t es m a -> (a -> t es' m b) -> t (es :++: es') m b

  (>!) :: (Monad m) => t es m a -> t es' m b -> t (es :++: es') m b
  t >! m = t >!= \_ -> m

throws :: forall (es :: [Type]) t m a . (MonadThrow t, Monad m) => m a -> t es m a 
throws = inject

throw :: (MonadIO m, Exception e, MonadThrow t) => e -> t '[e] m a
throw = inject . liftIO . E.throwIO

newtype Throws (es :: [Type]) (m :: Type -> Type) (a :: Type) where
  Throws :: m a -> Throws es m a

instance MonadThrow Throws where
  inject = Throws
  (Throws m) >!= f = Throws $ m >>= (strip . f)
  strip (Throws m) = m


data Handler e a = Exception e => Handler (e -> IO a)

infixr 5 :&&: 
data Handlers (es :: [Type]) (a :: Type) :: Type where
  (:&&:) :: Handler e a -> Handlers es' a -> Handlers (e ': es') a
  Nil :: Handlers '[] a 
  Only :: Handler e a -> Handlers '[e] a

strip' :: Handlers es a -> [E.Handler a]
strip' (Nil) = []
strip' (Handler f :&&: hs) = E.Handler f : strip' hs
strip' (Only (Handler f)) = [E.Handler f]

catches :: (MonadThrow t, Monad m) => t es m a -> Handlers es a -> IO a
catches t hs = E.catches (stripIO t) (strip' hs) 

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

-- test3 :: Throws (ManyExceptions :++: '[ LovelyException, ExampleException ]) IO ()
test3 = do
  throws @ManyExceptions action
  throw Heartbroken
  throw Example1
  throw Example2
  where
    (>>) = (>!) @Throws

action :: IO ()
action = do
  putStrLn "I'm quite a naughty function"
  putStrLn "Can throw so many things!"
