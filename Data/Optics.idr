module Data.Optics

import public Data.Optics.Lens
import public Data.Optics.Iso
import public Data.Optics.Prism
import public Data.Optics.Optional

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

infixr 5 <:+
(<:+) : (Prism a b) -> (Lens s a) -> (Optional s b)
(<:+) prism lens = let prismOptional = prismAsOptional prism
                       lensOptional = lensAsOptional lens
                       in prismOptional ?:+ lensOptional
