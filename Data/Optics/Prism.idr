module Data.Optics.Prism

||| Prism
data Prism s a =
  MkPrism (s -> Maybe a)  (a -> s)

%name Prism prism, prism1, prism2

%default total

to : Prism s a -> (s -> Maybe a)
to (MkPrism f g) s = f s

from: Prism s a -> (a -> s)
from (MkPrism f g) a = g a

infixr 9 <:+
||| compose two prisms
(<:+) : Prism a b -> Prism s a -> Prism s b
p1 <:+ p2 = MkPrism newTo newFrom
  where
    newTo: s -> Maybe b
    newTo s = case (to p2 s) of
              Nothing => Nothing
              Just a  => (to p1 a)
    newFrom: b -> s
    newFrom = with Prelude.Basics (from p2 . from p1)

infixr 9 :+>
(:+>) : Prism s a -> Prism a b -> Prism s b
(:+>) = flip (<:+)

--
-- Functions
--

||| focus on the left of an Either
left: Prism (Either a b) a
left = MkPrism to from
  where
    to: (Either a b) -> Maybe a
    to (Left x) = Just x
    to (Right x) = Nothing

    from: a -> (Either a b)
    from x = Left x

||| focus on the right of an Either
right: Prism (Either a b) b
right = MkPrism to from
  where
    to : Either a b -> Maybe b
    to (Left x) = Nothing
    to (Right x) = Just x

    from: b -> Either a b
    from x = Right x

||| focus on Just of a Maybe
just: Prism (Maybe a) a
just = MkPrism to from
  where
    to: Maybe a -> Maybe a
    to Nothing = Nothing
    to (Just x) = Just x

    from: a -> Maybe a
    from x = Just x
