-- Defaultable Typklasse Datei
-- Datum: 29.11.
module Defaultable where


-- Definiert eine Typklasse für Typen, die einen Standardwert (eine Liste von 
--  Werten) liefern können.
class Defaultable a where
  defaultValue :: [a] -- liefert eine Liste von Standardwerten des Typs 'a'.

instance Defaultable Int where
  defaultValue = [(-100)..100]

instance Defaultable Char where
  defaultValue = ['a'..'z'] ++ ['A'..'Z']
