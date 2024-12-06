-- Landeshauptstadt Typ Datei
-- Datum: 29.11.
module Landeshauptstadt where
import Defaultable
import Menge


data Landeshauptstadt = B | E | G | I | K | L | P | S | W
    deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- Man darf nicht `deriving` fuer `type` Deklarationen
--  nutzen, weil es nur ein Typsynonym ist
type Staedtepaar = (Landeshauptstadt, Landeshauptstadt)

data Staedtepaar1 = SP1 Landeshauptstadt Landeshauptstadt
    deriving (Eq, Ord, Show, Read, Bounded)

newtype Staedtepaar2 = SP2 (Landeshauptstadt, Landeshauptstadt)
    deriving (Eq, Ord, Show, Read, Bounded)

-- Man kann nicht `deriving` fuer Typen nutzen,
--  die charakteristische Funktionen sind
data Staedtepaar3 = SP3 ((Landeshauptstadt, Landeshauptstadt) -> Bool)
data Staedtepaar4 = SP4 (Landeshauptstadt -> Landeshauptstadt -> Bool)



instance Defaultable Landeshauptstadt where
    defaultValue = [minBound .. maxBound]

instance Defaultable Staedtepaar where
    defaultValue = [(a, b) | a <- defaultValue, b <- defaultValue]
