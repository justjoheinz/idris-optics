module Data.Optics.Iso

||| Isomorphism
data Iso: Type -> Type -> Type where
  ||| create an isomorphism between s and a
  MkIso: (s -> a) -> (a -> s) -> Iso s a

%name Iso iso, iso1, iso2

%default total

to: (Iso s a) -> s -> a
to (MkIso f g) s = f s

from: (Iso s a) -> a -> s
from (MkIso f g) a = g a

modify: (a -> a) -> (Iso s a) -> s -> s
modify f (MkIso to from) s = (from . f . to ) s

infixr 5 +:+
||| compose two Isomorphisms
(+:+): (Iso a b) -> (Iso s a) -> (Iso s b)
(+:+) x y = MkIso newTo newFrom
  where
    newTo: s -> b
    newTo  = (to x) . (to y)
    newFrom: b -> s
    newFrom = (from y) . (from x)
