-- Relation Typklasse Datei
-- Datum: 29.11.
module Relation where
import Menge


class Menge m => Relation m where
    istLeereRelation :: m -> Bool
    istAllRelation :: m -> Bool
    istLinkstotal :: m -> Bool
    istRechtstotal :: m -> Bool
    istReflexiv :: m -> Bool
    istSymmetrisch :: m -> Bool
    istTransitiv :: m -> Bool
    istQuasiOrdnung :: m -> Bool
    istAequivalenzrelation :: m -> Bool


    -- PROTOIMPLEMENTIERUNGEN --
    istLeereRelation = sindGleich leereMenge
    istQuasiOrdnung m = istReflexiv m && istTransitiv m
    istAequivalenzrelation m = istQuasiOrdnung m && istSymmetrisch m
