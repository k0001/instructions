{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Control.Instructions
  ( Run(..)

  , InsF
  , Ins
  , ins

  , Exe
  , exe

  , Arg
  , Ret

  , Program
  , ProgramF
  , Interpreter

  , (:+:)
  , (:*:)((:*:))
  , (:<:)
  )
where

import           Control.Monad.Trans.Free

--------------------------------------------------------------------------------

-- | Argument type expected by the instruction.
type family Arg (t :: k) :: *

-- | Return type expected by the command.
type family Ret (t :: k) :: *

---
-- | Description of how to interpret the instruction @t@ in the monad @m@.
newtype Exe (t :: k) (m :: * -> *) = Exe (Arg t -> m (Ret t))

exe :: (Arg t -> m (Ret t)) -> Exe t m
exe = Exe

---
-- | A functor for the instruction @t@.
--
-- To be used as the functor for a free monad.
newtype InsF (t :: k) (n :: *) = InsF (Arg t, Ret t -> n)
  deriving (Functor)

-- | Synonym for the type returned by @'ins' x@.
type Ins t f m = (MonadFree f m, InsF t :<: f, Functor f) => Arg t -> m (Ret t)

-- | Inject an instruction into the functor @f@ supporting, presumably a (':+:')
-- supporting more instructions.
ins
  :: forall proxy t f m
   . (MonadFree f m, InsF t :<: f, Functor f)
  => proxy t -> Arg t -> m (Ret t) -- ^
ins _ = \arg -> liftF (inj (InsF (arg, id) :: InsF t (Ret t)))

---

type Program (xs :: [k]) = FreeT (ProgramF xs)

-- | Build a sum of 'InsF' functors from each element in the list, resulting
-- in a functor suitable for being used as a free monad.
type family ProgramF (xs :: [k]) :: * -> * where
  ProgramF '[x]      = InsF x
  ProgramF (x ': xs) = InsF x :+: ProgramF xs

-- | Build a product of 'Exe's from each element in the list,
-- resulting in a functor suitable for being used as a cofree comonad.
type family Interpreter (xs :: [k]) :: (* -> *) -> * where
  Interpreter '[x]      = Exe x
  Interpreter (x ': xs) = Exe x :*: Interpreter xs

-- | Run @f@ in @m@ as described by @e@.
class Monad m => Run (m :: * -> *) (f :: * -> *) (e :: *) | e -> m where
  -- | Run @f@ in @m@ as described by @e@.
  --
  -- @
  -- 'run' :: 'Monad' m => 'Interpreter' [t, ...] m -> 'Program' [t, ...] m a -> m a
  -- 'run' :: 'Monad' m => 'Interpreter' [t, ...] m -> 'FreeT' ('ProgramF' [t, ...]) m a -> m a
  -- 'run' :: 'Monad' m => ('Exe' t :*: ...) m -> 'FreeT' ('InsF' t :+: ...) m a -> m a
  -- 'run' :: 'Monad' m => 'Exe' t m -> 'FreeT' ('InsF' t) m a -> m a
  -- @
  --
  -- Notice that the compiler will have trouble inferring @f@ if it was
  -- constructed using ':<:'. You should fix @f@ to a particular type.
  run :: e -> f a -> m a

instance Monad m => Run m (InsF t) (Exe t m) where
  run (Exe k) (InsF (a, fr)) = fr <$> k a
instance (Monad m, Run m f (ef m), Run m g (eg m)) => Run m (f :+: g) ((ef :*: eg) m) where
  run (e :*: _) (InL f) = run e f
  run (_ :*: e) (InR g) = run e g
instance (Monad m, Run m f (e m)) => Run m (FreeT f m) (e m) where
  run e = \f0 -> do
     x <- runFreeT f0
     case x of
        Pure a -> return a
        Free f -> run e =<< run e f

--------------------------------------------------------------------------------

infixr 6 :+:
-- | Like the poly-kinded 'Data.Functor.Sum.Sum' from @transformers >= 0.5@.
--
-- We will switch to @transformers >= 0.5@ once that version is widely deployed.
data (f :+: g) a = InL (f a) | InR (g a)
  deriving (Functor, Eq, Ord, Show)

infixr 8 :*:
-- | Like the poly-kinded 'Data.Functor.Product.Product' from @transformers >= 0.5@.
--
-- We will switch to @transformers >= 0.5@ once that version is widely deployed.
data (f :*: g) a = f a :*: g a
  deriving (Functor, Eq, Ord, Show)

infixl 5 :<:
-- | @f ':<:' g@ implies that given @f a@ you can construct @g a@.
class f :<: g where inj :: f a -> g a
instance f :<: f where inj = id
instance f :<: (f :+: g) where inj = InL
instance (f :<: h) => f :<: (g :+: h) where inj = InR . inj


