module Data.Match.Membership where

import Data.Kind (Type)

data Elem (f :: Type -> Type) (fs :: [Type -> Type]) where
  Here  :: Elem f (f ': fs)
  There :: Elem f fs -> Elem f (g ': fs)

class Mem f fs where
  witness :: Elem f fs

instance {-# overlaps #-} Mem f (f ': fs) where
  witness = Here

instance (Mem f fs) => Mem f (g ': fs) where
  witness = There witness
