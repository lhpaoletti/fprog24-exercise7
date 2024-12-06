-- MT3 Typ Datei
-- Datum: 29.11.
module MT3 where
import Menge
import Defaultable


data MT3 e = MT3 (e -> Bool)


instance (Eq e, Defaultable e, Show e) => Menge (MT3 e) where
    leereMenge = MT3 (\_ -> False)
    allMenge   = MT3 (\_ -> True )
    istMenge   = \_ -> True

    vereinige (MT3 f1) (MT3 f2) = MT3 $
        \elem -> f1 elem || f2 elem

    schneide (MT3 f1) (MT3 f2) = MT3 $
        \elem -> f1 elem && f2 elem

    zieheab (MT3 f1) (MT3 f2) = MT3 $
        \elem -> f1 elem && (not . f2) elem

    istTeilmenge m1 (MT3 f) =
        let elems1 = toList m1
        in all f elems1

    zeige m = "{" ++ (Menge.formatElems . toList) m ++ "}"



-- Wandle einen MT3 in einer Liste vom inneren Typ e um.
-- Sie benoetigt die Typklassebeschraenkung (Defaultable e), um sicherzustellen, 
--  dass der Typ e eine defaultValue Funktion besitzt.
-- Haette man nicht diese Beschaenkung, muesste man Bound nutzen. Es ist aber sehr
--  gross bei Int und Char.
toList :: (Defaultable e) => MT3 e -> [e]
toList (MT3 f) = filter f defaultValue
