module Data.Optics.Lens

import Data.Morphisms

||| Lens
data Lens s a =
  MkLens (s -> a) (s -> a -> s)

%name Lens lens, lens1, lens2

%default total

||| return the getter part of the lens
get: Lens s a -> (s -> a)
get (MkLens get set)  = get

||| return the setter part of the lens
set: Lens s a -> (s -> a -> s)
set (MkLens get set)  = set

||| modify the part of a structure
modify: (a -> a) -> Lens s a -> s -> s
modify f lens s =  set lens s (f . (get lens) $ s)

||| modify the part of a structure using an Endomorphism
modifyE: Endomorphism a -> (Lens s a) -> s -> s
modifyE (Endo f) lens = modify f lens

||| modify the part of a structure using a functor to do so
modifyF : Functor m => (a -> m a) -> Lens s a -> s -> m s
modifyF f lens s = map (set lens s) (f . (get lens) $ s)

||| modify the part of a structure using a kleisli morphism
modifyK: Monad m => Kleislimorphism m a a -> Lens s a -> (s -> m s)
modifyK kleisli lens = modifyF (applyKleisli kleisli) lens


--
--  Compositions
--

infixr 9 #:+
||| compose two lenses
(#:+) : Lens a b -> Lens s a -> Lens s b
lens2 #:+ lens1 = MkLens newGet newSet
  where
    newGet : s -> b
    newGet= get lens2 . get lens1

    newSet: s -> b -> s
    newSet s b = let a' = get lens1 s
                     newA = set lens2 a' b
                     newS = set lens1 s newA in
                     newS

infixr 9 +:#
(+:#) : Lens s a -> Lens a b -> Lens s b
(+:#) = flip (#:+)
