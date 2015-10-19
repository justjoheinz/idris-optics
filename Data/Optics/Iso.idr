module Data.Optics.Iso

import Data.Optics.Prism
import Data.Optics.Optional

||| Isomorphism
data Iso s a =
  ||| create an isomorphism between s and a
  MkIso  (s -> a) (a -> s)

%name Iso iso, iso1, iso2

%default total

to: (Iso s a) -> s -> a
to (MkIso f g) s = f s

from: (Iso s a) -> a -> s
from (MkIso f g) a = g a

modify: (a -> a) -> (Iso s a) -> s -> s
modify f (MkIso to from) s = (from . f . to ) s

--
-- Conversions
--

asOptional: (Prism s a) -> (Optional s a)
asOptional (MkPrism to from) = MkOptional (\s => to s) (\s,a => from a)

--
-- Compositions
--

infixr 5 -:+
||| compose two Isomorphisms
(-:+): (Iso a b) -> (Iso s a) -> (Iso s b)
(-:+) x y = MkIso newTo newFrom
  where
    newTo: s -> b
    newTo  = (to x) . (to y)
    newFrom: b -> s
    newFrom = (from y) . (from x)
