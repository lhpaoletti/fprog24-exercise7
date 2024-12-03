-- Relation Typklasse Datei
-- Datum: 29.11.
module Relation where
import Menge


class Menge m => Relation m where
    istLinkstotal  :: m -> Bool
    istRechtstotal :: m -> Bool
    istReflexiv    :: m -> Bool
    istSymmetrisch :: m -> Bool
    istTransitiv   :: m -> Bool

    istLeereRelation :: m -> Bool
    istLeereRelation = sindGleich leereMenge

    istAllRelation :: m -> Bool
    istAllRelation = sindGleich allMenge

    istQuasiOrdnung :: m -> Bool
    istQuasiOrdnung m = istReflexiv m && istTransitiv m

    istAequivalenzrelation :: m -> Bool
    istAequivalenzrelation m = istQuasiOrdnung m && istSymmetrisch m
