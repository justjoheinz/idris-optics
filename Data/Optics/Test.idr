module Data.Optics.Test

import Data.Optics.Lens
import Data.Optics.Prism

assertEq : Eq a => (given : a) -> (expected : a) -> IO ()
assertEq g e = if g == e
    then putStrLn "Test Passed"
    else putStrLn "Test Failed"

assertNotEq : Eq a => (given : a) -> (expected : a) -> IO ()
assertNotEq g e = if not (g == e)
    then putStrLn "Test Passed"
    else putStrLn "Test Failed"

mutual
  PLZ : Type
  PLZ = Integer

  record Person where
    constructor MkPerson
    name : String
    address : Address
    second : Maybe Address

  record Address where
    constructor MkAddress
    street: String
    plz : PLZ
    city : String

  instance Eq Person where
    (==) (MkPerson name address second) (MkPerson x y z) = (name == x) && (address == y) && (second == z)

  instance Eq Address where
    (==) (MkAddress street plz city) (MkAddress x y z) = (street == x) && (plz == y) && (city == z)

hamburg: Address
hamburg = MkAddress "Elbchaussee" 2000 "Hamburg"

p: Person
p = MkPerson "Holmes" hamburg (Just hamburg)

_name : Lens Person String
_name = MkLens (\p => name p) (\p,n => record { name = n} p)

_address : Lens Person Address
_address = MkLens (\p => address p) (\p, a => record { address = a} p)

_street : Lens Address String
_street = MkLens (\a => street a) (\a, s => record { street = s} a)

_person_street : Lens Person String
_person_street = _street +:+ _address

testPersonNameGet : IO ()
testPersonNameGet = assertEq (get _name p) ("Holmes")

testPersonNameSet : IO ()
testPersonNameSet = assertEq (set _name p "Watson") ( record { name = "Watson"} p)

testPersonAddressStreetGet : IO ()
testPersonAddressStreetGet = assertEq (get _person_street p) ("Elbchaussee")

testModifyF : IO ()
testModifyF = assertEq (modifyF (\n => Just n) _name p) (Just p)


runall : IO()
runall = do testPersonNameGet
            testPersonNameSet
            testPersonAddressStreetGet
            testModifyF
