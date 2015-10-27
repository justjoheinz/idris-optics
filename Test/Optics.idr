module Test.Optics

import UnitTest
import Data.Optics
import Control.Category

PLZ : Type
PLZ = Integer

mutual
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

  instance Show Person where
    show p = "Person(" ++ name p ++ ")"

  instance Show Address where
    show a = "Address(" ++ street a ++ ")"

hamburg: Address
hamburg = MkAddress "Elbchaussee" 2000 "Hamburg"

p: Person
p = MkPerson "Holmes" hamburg (Just hamburg)

_name : Lens Person String
_name = MkLens (\p => name p) (\p,n => record { name = n} p)

_address : Lens Person Address
_address = MkLens (\p => address p) (\p, a => record { address = a} p)

_secondary_address : Optional Person Address
_secondary_address = MkOptional (\p => second p) (\p, a => record { second = Just a} p )

_street : Lens Address String
_street = MkLens (\a => street a) (\a, s => record { street = s} a)

_person_street : Lens Person String
_person_street =  _address >>> _street

_secondary_street : Optional Person String
_secondary_street = _secondary_address +:? lensAsOptional _street

testPersonNameGet : IO ()
testPersonNameGet = assertEq "the _name lens can get the name of a person" (get _name p) ("Holmes")

testPersonNameSet : IO ()
testPersonNameSet = assertEq "the _name lens can set the name of a person" (set _name p "Watson") ( record { name = "Watson"} p)

testPersonAddressStreetGet : IO ()
testPersonAddressStreetGet = assertEq "the composed _person_street lens can get the name of the street" (get _person_street p) ("Elbchaussee")

testModifyF : IO ()
testModifyF = assertEq "modifyF can modify a value using a functor"(modifyF (\n => Just n) _name p) (Just p)


runall : IO()
runall = do testPersonNameGet
            testPersonNameSet
            testPersonAddressStreetGet
            testModifyF
