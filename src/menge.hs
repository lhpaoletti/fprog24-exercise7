-- Menge Typklasse Datei
-- Datum: 29.11.
module Menge where


type        Fehlermeldung = String
type MengeAlsZeichenreihe = String


class Menge m where
    leereMenge :: m
    allMenge :: m
    istMenge :: m -> Bool
    vereinige :: m -> m -> m
    schneide :: m -> m -> m
    zieheab :: m -> m -> m
    komplementiere :: m -> m
    sindGleich :: m -> m -> Bool
    sindUngleich :: m -> m -> Bool
    istTeilmenge :: m -> m -> Bool
    istObermenge :: m -> m -> Bool
    istEchteTeilmenge :: m -> m -> Bool
    istEchteObermenge :: m -> m -> Bool
    sindElementeFremd :: m -> m -> Bool
    sindQuerUeberlappend :: m -> m -> Bool
    istKeinGueltigerMengenwert :: Fehlermeldung -> m
    nichtImplementierbar :: Fehlermeldung -> m
    zeige :: m -> MengeAlsZeichenreihe


    -- PROTOIMPLEMENTIERUNGEN --

    komplementiere = zieheab allMenge

    -- Zwei Mengen sind gleich, wenn sie Teilmengen voneinander sind
    sindGleich m1 m2 = istTeilmenge m1 m2 && istTeilmenge m2 m1
    sindUngleich m1 = not . sindGleich m1

    -- Wenn A (echte) Obermenge von B ist, ist dann B (echte) Teilmenge von A
    istObermenge m1 m2 = istTeilmenge m2 m1
    istEchteObermenge m1 m2 = istEchteTeilmenge m2 m1 

    -- Eine Menge ist echte Teilmenge einer Anderen, wenn sie Teilmenge aber nicht 
    --  gleich ist
    istEchteTeilmenge m1 m2 = istTeilmenge m1 m2 && not (sindGleich m1 m2)

    -- Zwei Mengen sind elementefremd, wenn ihrer Schnitt die Leeremenge ist
    sindElementeFremd m1 = sindGleich leereMenge . schneide m1

    -- Zwei Mengen sind quer-ueberlappend, wenn sie...
    --   ... mindestens ein Element gemeinsam haben
    --   ... jeweils keine Teilmenge voneinander sind
    sindQuerUeberlappend m1 m2 =
        not (sindElementeFremd m1 m2)
        && not (istTeilmenge m1 m2)
        && not (istTeilmenge m2 m1)
    istKeinGueltigerMengenwert = error
    nichtImplementierbar = error



-- Formatiere Elemente, um sie auszudrucken.
formatElems :: Show a => [a] -> MengeAlsZeichenreihe
formatElems     [] = ""
formatElems    [e] = show e
formatElems (e:es) = show e ++ ", " ++ formatElems es

-- Fehlermeldung fuer wenn ein oder mehrere Argumente nicht Menge sind.
fehlermeldung :: a
fehlermeldung = error "Argument muss Menge sein (keine Duplikate)"
