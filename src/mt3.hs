-- MT3 Typ Datei
-- Datum: 29.11.
module MT3 where
import Menge
import Defaultable


data MT3 e = MT3 (e -> Bool)


instance Menge (MT3 Char) where
  leereMenge = MT3 (\_ -> False)
  allMenge = MT3 (\_ -> True )
  istMenge = \_ -> True
  vereinige = vereinigeMT3
  schneide = schneideMT3
  zieheab = zieheabMT3
  istTeilmenge = istTeilmengeMT3
  zeige = zeigeMT3

instance Menge (MT3 Int) where
  leereMenge = MT3 (\_ -> False)
  allMenge = MT3 (\_ -> True )
  istMenge = \_ -> True
  vereinige = vereinigeMT3
  schneide = schneideMT3
  zieheab = zieheabMT3
  istTeilmenge = istTeilmengeMT3
  zeige = zeigeMT3



vereinigeMT3 :: Eq e => MT3 e -> MT3 e -> MT3 e
vereinigeMT3 (MT3 f1) (MT3 f2) = MT3 $
    \elem -> f1 elem || f2 elem

schneideMT3 :: Eq e => MT3 e -> MT3 e -> MT3 e
schneideMT3 (MT3 f1) (MT3 f2) = MT3 $
    \elem -> f1 elem && f2 elem

zieheabMT3 :: Eq e => MT3 e -> MT3 e -> MT3 e
zieheabMT3(MT3 f1) (MT3 f2) = MT3 $
    \elem -> f1 elem && (not . f2) elem

istTeilmengeMT3 :: (Eq e, Defaultable e) => MT3 e -> MT3 e -> Bool
istTeilmengeMT3 m1 (MT3 f) =
    let elems1 = toListMT3 m1
    in all f elems1

zeigeMT3 m = "{" ++ (Menge.formatElems . toListMT3) m ++ "}"


-- Wandle einen MT3 in einer Liste vom inneren Typ e um.
-- Sie benoetigt die Typklassebeschraenkung (Defaultable e), um sicherzustellen, 
--  dass der Typ e eine defaultValue Funktion besitzt.
-- Haette man nicht diese Beschaenkung, muesste man Bound nutzen. Es ist aber sehr
--  gross bei Int und Char.
toListMT3 :: (Defaultable e) => MT3 e -> [e]
toListMT3 (MT3 f) = filter f defaultValue
