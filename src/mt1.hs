-- MT1 Typ Datei
-- Datum: 29.11.
module MT1 where
import Menge
import Defaultable
import Landeshauptstadt
import Relation


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



instance Relation (MT1 Staedtepaar) where
    istLinkstotal (MT1 pairs) =
        -- Bilde eine Menge mit den linken Landeshauptstaedten, und vergleiche sie mit der Allmenge
        sindGleich allMenge . MT1 . nub . map (fst) $ pairs

    istRechtstotal (MT1 pairs) =
        -- Bilde eine Menge mit den rechten Landeshauptstaedten, und vergleiche sie mit der Allmenge
        sindGleich allMenge . MT1 . nub . map (snd) $ pairs

    istReflexiv (MT1 pairs) =
        -- Bilde eine Menge mit den reflexiven Landeshauptstaedten,
        --  und vergleiche sie mit der Allmenge
        sindGleich allMenge . MT1 $ reflexiveCities
        where
            reflexiveCities = [fst pair | pair <- pairs, fst pair == snd pair]


    istSymmetrisch r = istSymmetrisch' r r
        where
            -- Pruefe Symmetrie von m gegen die urspruengliche Menge.
            istSymmetrisch' :: MT1 Staedtepaar -> MT1 Staedtepaar -> Bool
            istSymmetrisch'         _  (MT1 []) = True
            istSymmetrisch' (MT1 orig) (MT1 ((l, l'):ps)) =
                -- Schaue ob die Symmetrie fuer das erste Paar gilt, dann schau weiter
                (l', l) `elem` orig && (istSymmetrisch' (MT1 orig) . MT1) ps


    istTransitiv r = istTransitiv' r r
        where
            -- Pruefe Transitivitaet von m gegen die urspruengliche Menge.
            istTransitiv' :: MT1 Staedtepaar -> MT1 Staedtepaar -> Bool
            istTransitiv'         _  (MT1 []) = True
            istTransitiv' (MT1 orig) (MT1 ((l, l'):ps)) =
                -- Schaue ob die Transitivitaet aller (l R l'')s gilt, dann schau weiter
                all (`elem` orig) l_l''s && (istTransitiv' (MT1 orig) . MT1) ps
                where
                    -- Alle vorgegebene (l' R x)s
                    l'_l''s = filter (\pair -> fst pair == l') orig
                    l''s    = map (snd) l'_l''s
                    -- Alle zu pruefende Transitivitaeten aus l
                    l_l''s  = [(l, l'') | l'' <- l''s]



-- Entferne Duplikate einer Liste.
nub :: Eq a => [a] -> [a]
nub     [] = []
nub (e:es) = e : (nub $ filter (/= e) es)
