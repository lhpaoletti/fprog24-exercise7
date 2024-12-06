-- MT1 Typ Datei
-- Datum: 29.11.
module MT1 where
import Menge
import Defaultable


newtype MT1 e = MT1 [e] 


instance (Eq e, Defaultable e, Show e) => Menge (MT1 e) where
    leereMenge = MT1 []
    allMenge   = MT1 defaultValue

    istMenge (MT1     []) = True
    istMenge (MT1 (e:es)) =
        all (/= e) es && (istMenge . MT1) es

    vereinige m1@(MT1 list1) m2@(MT1 list2)
          | istMenge m1 && istMenge m2 = MT1 . nub $ list1 ++ list2
          | otherwise = istKeinGueltigerMengenwert "Argument muss Menge sein (keine Duplikate)"

    schneide m1@(MT1 list1) m2@(MT1 list2)
          | istMenge m1 && istMenge m2 = MT1 $ [e | e <- list1, e `elem` list2]
          | otherwise = istKeinGueltigerMengenwert "Argument muss Menge sein (keine Duplikate)"

    zieheab m1@(MT1 list1) m2@(MT1 list2)
          | istMenge m1 && istMenge m2 = MT1 $ [e | e <- list1, e `notElem` list2]
          | otherwise = istKeinGueltigerMengenwert "Argument muss Menge sein (keine Duplikate)"

    istTeilmenge m1@(MT1 list1) m2@(MT1 list2)
          | istMenge m1 && istMenge m2 = all (`elem` list2) list1
          | otherwise = error "Argument muss Menge sein (keine Duplikate)"

    zeige (MT1 elems) = "{" ++ Menge.formatElems elems ++ "}"



-- Entferne Duplikate einer Liste.
nub :: Eq a => [a] -> [a]
nub     [] = []
nub (e:es) = e : (nub $ filter (/= e) es)
