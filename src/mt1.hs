-- MT1 Typ Datei
-- Datum: 29.11.
module MT1 where
import Menge
import Defaultable


newtype MT1 e = MT1 [e] 


instance Menge (MT1 Char) where
    leereMenge = MT1 []
    allMenge   = MT1 defaultValue
    istMenge = istMengeMT1
    vereinige = vereinigeMT1
    schneide  =  schneideMT1
    zieheab   =   zieheabMT1
    istTeilmenge = istTeilmengeMT1
    zeige = zeigeMT1

instance Menge (MT1 Int) where
    leereMenge = MT1 []
    allMenge   = MT1 defaultValue
    istMenge = istMengeMT1
    vereinige = vereinigeMT1
    schneide  =  schneideMT1
    zieheab   =   zieheabMT1
    istTeilmenge = istTeilmengeMT1
    zeige = zeigeMT1



istMengeMT1 :: Eq e => MT1 e -> Bool
istMengeMT1 (MT1     []) = True
istMengeMT1 (MT1 (e:es)) =
    all (/= e) es && (istMengeMT1 . MT1) es

vereinigeMT1 :: Eq e => MT1 e -> MT1 e -> MT1 e
vereinigeMT1 m1@(MT1 list1) m2@(MT1 list2)
      | istMengeMT1 m1 && istMengeMT1 m2 = MT1 . nub $ list1 ++ list2
      | otherwise                        = Menge.fehlermeldung

schneideMT1 :: Eq e => MT1 e -> MT1 e -> MT1 e
schneideMT1 m1@(MT1 list1) m2@(MT1 list2)
      | istMengeMT1 m1 && istMengeMT1 m2 = MT1 $ [e | e <- list1, e `elem` list2]
      | otherwise                        = Menge.fehlermeldung

zieheabMT1 :: Eq e => MT1 e -> MT1 e -> MT1 e
zieheabMT1 m1@(MT1 list1) m2@(MT1 list2)
      | istMengeMT1 m1 && istMengeMT1 m2 = MT1 $ [e | e <- list1, e `notElem` list2]
      | otherwise                        = Menge.fehlermeldung


istTeilmengeMT1 :: Eq e => MT1 e -> MT1 e -> Bool
istTeilmengeMT1 m1@(MT1 list1) m2@(MT1 list2)
      | istMengeMT1 m1 && istMengeMT1 m2 = all (`elem` list2) list1
      | otherwise                        = Menge.fehlermeldung

zeigeMT1 :: Show e => MT1 e -> MengeAlsZeichenreihe
zeigeMT1 (MT1 elems) = "{" ++ Menge.formatElems elems ++ "}"



-- Entferne Duplikate einer Liste.
nub :: Eq a => [a] -> [a]
nub     [] = []
nub (e:es) = e : (nub $ filter (/= e) es)
