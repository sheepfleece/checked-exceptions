{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
-- | An example is worth a thousand words:
--
-- * https://github.com/sheepfleece/checked-exceptions/blob/master/examples/Simple.hs
--
-- * https://github.com/sheepfleece/checked-exceptions/blob/master/examples/Reader.hs
--
module Control.Monad.Throw (
    MonadThrow(..)
  , Throws(..)
  , Handler(..)
  , Handlers(..)
  , throw
  , throws
  , catches
  , catches'
  , ignore
  , ignore'
  , erode
  , erode'
  , safe
  , escape
  , (>!=)
  , (>!)
  , cond
  , result
  , (:++:)
  , (:\\:)
  , (:\\!:)
) where

import           Control.Exception      (Exception)
import qualified Control.Exception      as E
import           Control.Monad.IO.Class
import           Data.Coerce
import           Data.Kind
import           GHC.TypeLits

import           Control.Monad.Reader

-- | The main typeclass which stores a type level list describing which
-- exceptions your computation may throw. Sadly, GHC does not provide us
-- type level sets, so if compiler infers exceptions in the different
-- order they appear in your type signature you will get an ugly error message.
-- You can, however, let GHC infer the type and then copy it into your type signature.

class MonadIO m => MonadThrow (t :: [Type] -> (Type -> Type) -> (Type -> Type)) (m :: Type -> Type) where
  inject :: forall (es :: [Type]) a . m a -> t es m a
  strip :: t es m a -> m a

  catch :: Exception e => t '[e] m a -> (e -> m a) -> m a

-- | RebindableSyntax for @(>>)@.
(>!) :: forall t m a b (es :: [Type]) (es' :: [Type])
     .  MonadThrow t m
     => t es m a -> t es' m b -> t (es :++: es') m b
t >! m = t >!= \_ -> m

-- | RebindableSyntax for @(>>=)@.
(>!=) :: forall t m a b (es :: [Type]) (es' :: [Type])
      .  MonadThrow t m
      => t es m a -> (a -> t es' m b) -> t (es :++: es') m b
t >!= f = inject $ strip t >>= (strip . f)

-- | RebindableSyntax for @return@.
result :: (MonadThrow t m) => a -> t '[] m a
result = inject . pure

-- | RebindableSyntax for @if..then..else@
cond :: forall t m a (es :: [Type]) (es' :: [Type])
     .  (MonadThrow t m)
     => Bool -> t es m a -> t es' m a -> t (es :++: es') m a
cond True t _  = inject . strip $ t
cond False _ f = inject . strip $ f

-- | Forget about some exceptions in the computation.
ignore :: forall (es' :: [Type]) t m a (es :: [Type])
        . (MonadThrow t m)
        => t es m a -> t (es :\\: es') m a
ignore = inject . strip

-- | The same as 'ignore' but allows you to ignore not present errors.
--
-- > type Exceptions = '[ ArithException, ArrayException ]
-- >
-- > action :: Throws '[ ArrayException ] IO a
-- >
-- > ignore' @Exceptions action -- compiles
-- > ignore  @Exceptions action -- does not
--
--   Should be useful with type synonyms for lists of exceptions.
ignore' :: forall (es' :: [Type]) t m a (es :: [Type])
        . (MonadThrow t m) => t es m a -> t (es :\\!: es') m a
ignore' = inject . strip

-- | The same as 'inject' but with an order of type variables swapped,
-- which synergizes nicely with `TypeApplications`.
--
-- > throws @'[ ArithException ] (putStrLn "naughty")
throws :: forall (es :: [Type]) t m a . (MonadThrow t m, Monad m) => m a -> t es m a
throws = inject

-- | Throw an exception and add it to a type level list.
throw :: (MonadIO m, Exception e, MonadThrow t m) => e -> t '[e] m a
throw = inject . liftIO . E.throwIO

-- | 'Handler' stores a type of 'Exception' it handles in its type signature
-- , it allows to match a list of provided handlers against a list of
--  exceptions possible inside a computation and throw an error
--  if they do not match.
data Handler e m a where
  Handler :: Exception e => (e -> m a) -> Handler e m a

-- | A type level list of `Handler`s.
data Handlers (es :: [Type]) (m :: Type -> Type) (a :: Type) :: Type where
  (:&&:) :: Exception e
         => Handler e m a
         -> Handlers es' m a
         -> Handlers (e ': es') m a
  Nil :: Handlers '[] m a
infixr 5 :&&:

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

type family Covers' (es :: [k]) (hs :: [k]) :: Constraint where
  Covers' '[] _ = ()
  Covers' (a ': as) bs =
    If (Member a bs)
      (Covers' as (Remove a bs))
      (TypeError
        ('Text "Not handled exception: " ':<>: 'ShowType a))


-- | Combine two type level lists into one.
-- If both lists have the same element it will be stored only once.
infixr 5 :++:
type family (:++:) (a :: [k]) (b :: [k]) :: [k] where
  '[] :++: bs = bs
  (a:as) :++: bs = If (Member a bs) (as :++: bs) (a ': (as :++: bs))

-- | The list difference.
-- It is a type error to try to remove an element
-- from the second list which is not present in the first.
--
-- This way you may be sure you aren't ignoring impossible
-- (i.e. not specified in your type) errors.
infixl 4 :\\:
type family (:\\:) (as :: [k]) (bs :: [k]) :: [k] where
  as :\\: '[] = as
  as :\\: (b ': bs) =
    If (Member b as)
      (Remove b as :\\: bs)
      (TypeError
        ('Text "An exception " ':<>: 'ShowType b
         ':<>: 'Text " is not present in the list of possible exceptions "
         ':<>: 'ShowType as))

-- | The list difference with relaxed signature.
-- Allows you to have elements in the second argument
-- which are not present in the first.
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

-- | The main type to be used with the `MonadCatch` typeclass
--   `m` can be instantiated to your own Monad, RIO as an example.
newtype Throws (es :: [Type]) (m :: Type -> Type) (a :: Type) where
  Throws :: m a -> Throws es m a
  deriving Functor

instance MonadThrow Throws IO where
  inject = coerce
  strip  = coerce

  catch :: Exception e => Throws '[e] IO a -> (e -> IO a) -> IO a
  catch t f = E.catch (strip t) f

instance (MonadIO m, MonadThrow Throws m) => MonadThrow Throws (ReaderT r m) where
  inject = coerce
  strip  = coerce

  catch :: ((ReaderT r m) ~ t, Exception e)
        => Throws '[e] t a
        -> (e -> t a)
        -> t a
  catch (Throws (ReaderT m)) f =
    ReaderT $ \r -> Throws (m r) `catch` (\e -> runReaderT (f e) r)

-- |  Catch all exceptions inside a computation.
--
-- > action :: Throws '[ ArithException, ArrayException ] IO ()
-- > catches action (Handler f1 :&&: Handler f2 :&&: Nil)
-- > catches action (Handler f2 :&&: Handler f1 :&&: Nil) -- also compiles
catches :: forall t m a (es :: [Type]) (hs :: [Type])
        . (MonadThrow t m, Covers es hs)
        => t es m a
        -> Handlers hs m a
        -> m a
catches t hs = (inject @t . strip $ t) `catch` pick hs

pick :: Handlers hs m a -> E.SomeException -> m a
pick Nil e = E.throw e
pick (Handler f :&&: hs) e =
  case E.fromException e of
    Nothing -> pick hs e
    Just e' -> f e'

-- | The same as 'catches', but allows to add handlers for
-- non-specified exceptions.
catches' :: forall t m a (es :: [Type]) (hs :: [Type])
        . (MonadThrow t m, Covers' es hs)
        => t es m a
        -> Handlers hs m a
        -> m a
catches' t hs = (inject @t . strip $ t) `catch` pick hs


-- | Handle some exceptions without leaving the monad.
erode :: forall t m a (es :: [Type]) (hs :: [Type])
      .  (MonadThrow t m)
      => t es m a
      -> Handlers hs m a
      -> t (es :\\: hs) m a
erode t hs = inject @t $ (inject @t . strip $ t) `catch` pick hs

-- | The same as 'erode', but allows to add handlers
-- for non-specified exceptions.
erode' :: forall t m a (es :: [Type]) (hs :: [Type])
      .  (MonadThrow t m)
      => t es m a
      -> Handlers hs m a
      -> t (es :\\!: hs) m a
erode' t hs = inject @t $ (inject @t . strip $ t) `catch` pick hs

safe :: forall t m a . (MonadThrow t m) => m a -> t '[] m a
safe = inject

escape :: forall t m a . (MonadThrow t m) => t '[] m a -> m a
escape = strip
