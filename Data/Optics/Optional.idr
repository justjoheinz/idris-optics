module Data.Optics.Optional

import Control.Category

data Optional s a =
  MkOptional (s -> Maybe a) (s -> a -> s)

%name Optional optional, optional1, optional2

get : (Optional s a) -> (s -> Maybe a)
get (MkOptional get set) = get

getOrModify: (Optional s a) -> s -> Either s a
getOrModify optional s' = case (get optional s') of
                          Just a' => Right a'
                          Nothing => Left s'

set : Optional s a -> s -> a -> s
set (MkOptional get set)  = set

--
-- Compositions
--

infixr 5 ?:+
||| compose two optionals
(?:+): Optional a b -> Optional s a -> Optional s b
(?:+) (MkOptional get1 set1) (MkOptional get2 set2) = MkOptional newGet newSet
  where
    newGet: s -> Maybe b
    newGet s = do a <- get2 s
                  b <- get1 a
                  return b

    newSet: s -> b -> s
    newSet s b = let a' = get2 s
                     newA = do a'' <- a'
                               return (set1 a'' b)
                     newS = case newA of
                              Just v => set2 s v
                              Nothing => s
                     in newS

infixr 5 +:?
(+:?) : Optional s a -> Optional a b -> Optional s b
(+:?) = flip (?:+)

instance Category Optional where
  id = MkOptional Just (\a => id)
  (.) = (?:+)
