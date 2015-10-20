module Data.Optics.Iso

import Data.Optics.Prism
import Data.Optics.Optional
import Data.Optics.Lens
import Control.Category

||| Isomorphism
data Iso s a =
  ||| create an isomorphism between s and a
  MkIso  (s -> a) (a -> s)

%name Iso iso, iso1, iso2

%default total

to: Iso s a -> s -> a
to (MkIso f g) s = f s

from: Iso s a -> a -> s
from (MkIso f g) a = g a

modify: (a -> a) -> (Iso s a) -> s -> s
modify f (MkIso to from) s = with Prelude.Basics (from . f . to ) s

--
-- Compositions
--

infixr 5 -:+
||| compose two Isomorphisms
(-:+): Iso a b -> Iso s a -> Iso s b
(-:+) x y = MkIso newTo newFrom
  where
    newTo: s -> b
    newTo  = with Prelude.Basics (to x . to y)
    newFrom: b -> s
    newFrom = with Prelude.Basics (from y . from x)

infixr 5 +:-
(+:-) : (Iso s a) -> (Iso a b) -> (Iso s b)
(+:-) = flip (-:+)

--
-- instances
--

instance Category Iso where
  id = MkIso id id
  (.) = (-:+)
