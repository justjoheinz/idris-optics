module Data.Optics

import public Data.Optics.Lens
import public Data.Optics.Iso
import public Data.Optics.Prism
import public Data.Optics.Optional

import Control.Category

--
-- Conversions
--

-- Iso

isoAsPrism: (Iso s a) -> (Prism s a)
isoAsPrism (MkIso to from) = MkPrism (\s => Just (to s)) (\a => from a)

isoAsLens: (Iso s a) -> (Lens s a)
isoAsLens (MkIso to from) = MkLens (to) (\s => from)

isoAsOptional: (Iso s a) -> (Optional s a)
isoAsOptional (MkIso to from) = MkOptional (\s =>  Just(to s)) (\s,a => from a)

-- Lens


lensAsOptional: (Lens s a ) -> (Optional s a)
lensAsOptional (MkLens get set) = MkOptional (\s => Just (get s)) (set)

-- Prism

prismAsOptional: Prism s a -> Optional s a
prismAsOptional (MkPrism to from) = MkOptional (to) (\s => from)


--
--  Compositions
--

infixr 9 <:+
||| the composition of a prism with a lens is an optional
(<:+) : Prism a b -> Lens s a -> Optional s b
(<:+) prism lens = let prismOptional = prismAsOptional prism
                       lensOptional = lensAsOptional lens
                       in prismOptional ?:+ lensOptional

infixr 9 +:>
(+:>) : Lens s a -> Prism a b -> Optional s b
(+:>) = flip (<:+)

--
-- instances
--

||| An Iso forms a Category
instance Category Iso where
 id = MkIso id id
 (.) = (-:+)

||| A Lens forms a Category
instance Category Lens where
 id = MkLens (id) (\a => id)
 (.) = (#:+)

||| An Optonal forms a Category
instance Category Optional where
 id = MkOptional Just (\a => id)
 (.) = (?:+)

||| A Prism forms a Category
instance Category Prism where
 id = MkPrism Just id
 (.) = (<:+)
