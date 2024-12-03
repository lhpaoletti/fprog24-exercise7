-- Menge Typklasse Datei
-- Datum: 29.11.
module Menge where


type        Fehlermeldung = String
type MengeAlsZeichenreihe = String


class Menge m where
    leereMenge :: m
    allMenge   :: m
    istMenge :: m -> Bool
    vereinige :: m -> m -> m
    schneide  :: m -> m -> m
    zieheab   :: m -> m -> m
    istTeilmenge :: m -> m -> Bool
    zeige :: m -> MengeAlsZeichenreihe

    komplementiere :: m -> m
    komplementiere = zieheab allMenge

    sindGleich :: m -> m -> Bool
    sindGleich m1 m2 = istTeilmenge m1 m2 && istTeilmenge m2 m1

    sindUngleich :: m -> m -> Bool
    sindUngleich m1 = not . sindGleich m1

    istObermenge :: m -> m -> Bool
    istObermenge = flip istTeilmenge

    istEchteTeilmenge :: m -> m -> Bool
    istEchteTeilmenge m1 m2 =
        istTeilmenge m1 m2 && not (sindGleich m1 m2)

    istEchteObermenge :: m -> m -> Bool
    istEchteObermenge = flip istEchteTeilmenge

    sindElementeFremd :: m -> m -> Bool
    sindElementeFremd m1 = sindGleich leereMenge . schneide m1

    sindQuerUeberlappend :: m -> m -> Bool
    sindQuerUeberlappend m1 m2 =
        not (sindElementeFremd m1 m2)
        && not (istTeilmenge m1 m2)
        && not (istTeilmenge m2 m1)

    istKeinGueltigerMengenwert :: Fehlermeldung -> m
    istKeinGueltigerMengenwert = error

    nichtImplementierbar :: Fehlermeldung -> m
    nichtImplementierbar = error



-- Formatiere Elemente, um sie auszudrucken.
formatElems :: Show a => [a] -> MengeAlsZeichenreihe
formatElems     [] = ""
formatElems    [e] = show e
formatElems (e:es) = show e ++ ", " ++ formatElems es

-- Fehlermeldung fuer wenn ein oder mehrere Argumente nicht Menge sind.
fehlermeldung :: a
fehlermeldung = error "Argument muss Menge sein (keine Duplikate)"
