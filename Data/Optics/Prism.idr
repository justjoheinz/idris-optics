module Data.Optics.Prism

||| Prism
data Prism: Type -> Type -> Type where
  MkPrism: (s -> Maybe a) -> (a -> s) -> Prism s a

%name Prism prism, prism1, prism2

%default total

to : (Prism s a) -> (s -> Maybe a)
to (MkPrism f g) = f

from: (Prism s a) -> (a -> s)
from (MkPrism f g) = g

infixr 5 +:+
||| compose two prisms
(+:+) : (Prism a b) -> (Prism s a) -> (Prism s b)
(+:+) (MkPrism toB fromB) (MkPrism toA fromS) = MkPrism sb bs
  where
    sb: s -> Maybe b
    sb s = case (toA s) of
              Nothing => Nothing
              Just a  => (toB a)
    bs: b -> s
    bs = fromS . fromB

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
