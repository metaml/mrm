module Data.ConstrainedList where

import Data.Foldable (Foldable())
import Data.Kind (Type)
import Data.Traversable (Traversable())

data FoldableList (fs :: [Type -> Type]) where
  FNil :: FoldableList '[]
  FCons :: (Functor f, Foldable f) => FoldableList fs -> FoldableList (f ': fs)

class Foldables (fs :: [Type -> Type]) where
  frep :: FoldableList fs

instance Foldables '[] where
  frep = FNil

instance (Functor f, Foldable f, Foldables fs) => Foldables (f ': fs) where
  frep = FCons frep

data TraversableList (fs :: [Type -> Type]) where
  TNil :: TraversableList '[]
  TCons :: Traversable f => TraversableList fs -> TraversableList (f ': fs)

class Traversables (fs :: [Type -> Type]) where
  trep :: TraversableList fs

instance Traversables '[] where
  trep = TNil

instance (Traversable f, Traversables fs) => Traversables (f ': fs) where
  trep = TCons trep
