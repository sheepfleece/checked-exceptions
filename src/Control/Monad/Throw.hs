{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Control.Monad.Throw (
    (:++:)
  , MonadThrow(..)
  , Throws(..)
  , Handler(..)
  , Handlers(..)
  , throw
  , throws
  , catch
  , catches
  , catches'
  , ignore
  , ignore'
) where

import           Control.Exception      (Exception)
import qualified Control.Exception      as E
import           Control.Monad.IO.Class
import           Data.Kind
import           GHC.TypeLits

{- |
The main type class which stores a type level list
describing which exceptions your computation may throw.

Sadly, GHC does not provide us type level sets, so if compiler infers
exceptions in the different order they appear in your type signature
you will get an ugly error message.
You can, however, let GHC infer the type and then copy it into your type signature.
-}
class MonadThrow (t :: [Type] -> (Type -> Type) -> (Type -> Type)) (m :: Type -> Type) where
  -- | 'inject' into a typeclass with specified list of exceptions.
  inject :: forall (es :: [Type]) a . m a -> t es m a
  -- | Forget everything.
  strip :: t es m a -> m a

  -- | Allows us to define 'throw' in terms of standard one in `Monad.Exception`.
  --   In theory you can also side effect here and do something meaningful.
  stripIO :: t es m a -> IO a

  -- | RebindableSyntax for @(>>=)@.
  (>!=) :: (Monad m) => t es m a -> (a -> t es' m b) -> t (es :++: es') m b
  t >!= f = inject $ strip t >>= (strip . f)

  -- | RebindableSyntax for @(>>)@.
  (>!) :: (Monad m) => t es m a -> t es' m b -> t (es :++: es') m b
  t >! m = t >!= \_ -> m

  -- | RebindableSyntax for @return@.
  pure1 :: (Monad m) => m a -> t '[] m a
  pure1 = inject

{- |
Forget about some exceptions in the computation.
-}
ignore :: forall (es' :: [Type]) t m a (es :: [Type]) . (MonadThrow t m) => t es m a -> t (es :\\: es') m a
ignore = inject . strip

{- |
The same as 'ignore' but allows you to ignore not present errors.

> type Exceptions = '[ ArithException, ArrayException ]
>
> action :: Throws '[ ArrayException ] IO a
>
> ignore' @Exceptions action -- compiles
> ignore  @Exceptions action -- does not

Should be useful with type synonyms for lists of exceptions.
-}
ignore' :: forall (es' :: [Type]) t m a (es :: [Type]) . (MonadThrow t m) => t es m a -> t (es :\\!: es') m a
ignore' = inject . strip

{- |
The same as 'inject' but with an order of type variables swapped, which synergizes nicely with `TypeApplications`.

> throws @'[ ArithException ] (putStrLn "naughty")
-}
throws :: forall (es :: [Type]) t m a . (MonadThrow t m, Monad m) => m a -> t es m a
throws = inject

{- |
Throw an exception and add it to a type level list.
-}
throw :: (MonadIO m, Exception e, MonadThrow t m) => e -> t '[e] m a
throw = inject . liftIO . E.throwIO

{- |
'Handler' stores a type of 'Exception' it handles in its type signature, it allows
to match a list of provided handlers against a list of exceptions possible
inside a computation and throw an error if they do not match.
-}
data Handler e a where
  Handler :: Exception e => (e -> IO a) -> Handler e a

{- |
A type level list of `Handler`s.
-}
data Handlers (es :: [Type]) (a :: Type) :: Type where
  (:&&:) :: Exception e => Handler e a -> Handlers es' a -> Handlers (e ': es') a
  Nil :: Handlers '[] a
  Only :: Exception e => Handler e a -> Handlers '[e] a

infixr 5 :&&:

{- |
Catch all exceptions inside a computation.

> action :: Throws '[ ArithException, ArrayException ] IO ()
> catches action (Handler f1 :&&: Handler f2 :&&: Nil)
> catches action (Handler f2 :&&: Handler f1 :&&: Nil) -- also compilers
-}
catches :: (MonadThrow t m, Monad m, Covers es hs) => t es m a -> Handlers hs a -> IO a
catches t hs = E.catches (stripIO t) (strip' hs)

{- |
The same as 'catches', but allows to add handlers for non-specified exceptions.
-}
catches' :: (MonadThrow t m, Monad m, Covers' es hs) => t es m a -> Handlers hs a -> IO a
catches' t  hs = E.catches (stripIO t) (strip' hs)

type family Covers (es :: [k]) (hs :: [k]) :: Constraint where
  Covers '[] '[] = ()
  Covers '[] bs  =
      (TypeError
        ('Text "Redundant handlers for " ':<>: 'ShowType bs
         ':<>: 'Text ". Use catches' if it is intentional"))
  Covers (a ': as) bs =
    If (Member a bs)
      (Covers as (Remove a bs))
      (TypeError
        ('Text "Not handled exception: " ':<>: 'ShowType a))

-- It would be a pain to factor a pattern out.
type family Covers' (es :: [k]) (hs :: [k]) :: Constraint where
  Covers' '[] _ = ()
  Covers' (a ': as) bs =
    If (Member a bs)
      (Covers' as (Remove a bs))
      (TypeError
        ('Text "Not handled exception: " ':<>: 'ShowType a))

{- |
For computations which `throw` only one exception.
-}
catch :: (MonadThrow t m, Monad m) => t '[e] m a -> Handler e a -> IO a
catch t (Handler f) = E.catches (stripIO t) [E.Handler f]

strip' :: Handlers es a -> [E.Handler a]
strip' (Nil)               = []
strip' (Handler f :&&: hs) = E.Handler f : strip' hs
strip' (Only (Handler f))  = [E.Handler f]

{- |
Combine two type level lists into one.
If both lists have the same element it will be stored only once.
-}
infixr 5 :++:
type family (:++:) (a :: [k]) (b :: [k]) :: [k] where
  '[] :++: bs = bs
  (a:as) :++: bs = If (Member a bs) (as :++: bs) (a ': (as :++: bs))

{- |
The list difference.
It is a type error to try to remove an element from the second list
which is not present in the first.

This way you may be sure you aren't ignoring impossible (from your type) errors.
-}
infixl 4 :\\:
type family (:\\:) (as :: [k]) (bs :: [k]) :: [k] where
  as :\\: '[] = as
  as :\\: (b ': bs) =
    If (Member b as)
      (Remove b as :\\: bs)
      (TypeError
        ('Text "An exception " ':<>: 'ShowType b
         ':<>: 'Text " is not present in the list of possible exceptions " ':<>: 'ShowType as))

{-|
The list difference with relaxed signature.
Allows you to have elements in the second argument
which are not present in the first.
-}
infixl 4 :\\!:
type family (:\\!:) (as :: [k]) (bs :: [k]) :: [k] where
  as :\\!: '[] = as
  as :\\!: (b ': bs) = Remove b as :\\!: bs

type family Remove (a :: k) (as :: [k]) :: [k] where
  Remove a (a ': as) = as
  Remove a (b ': bs) = b ': Remove a bs
  Remove a '[] = '[]

type family Member (a :: k) (as :: [k]) :: Bool where
  Member a '[] = 'False
  Member a (a ': _) = 'True
  Member a (_ ': as) = Member a as

type family If (p :: Bool) (a :: k) (b :: k) :: k where
  If 'True a _ = a
  If 'False _ b = b


{- |
The main type to be used with the `MonadCatch` typeclass
`m` can be instantiated to your own Monad, `RIO` as an example.
-}
newtype Throws (es :: [Type]) (m :: Type -> Type) (a :: Type) where
  Throws :: m a -> Throws es m a

instance MonadThrow Throws IO where
  inject = Throws
  strip (Throws m) = m
  stripIO = strip

