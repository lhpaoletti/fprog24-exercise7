-- IstPartnerStadtVon1 Typ Datei
-- Datum: 02.12.
module IPV1 where
import Landeshauptstadt
import Menge
import Relation
import MT1


data IstPartnerstadtVon1 = IPV1 (MT1 Staedtepaar)


instance Menge (MT1 Staedtepaar) where
    leereMenge = MT1 []
    allMenge = cross allMenge allMenge
    istMenge = istMengeMT1
    vereinige = vereinigeMT1
    schneide = schneideMT1
    zieheab = zieheabMT1
    istTeilmenge = istTeilmengeMT1
    zeige = zeigeMT1

instance Menge IstPartnerstadtVon1 where
    leereMenge = IPV1 leereMenge
    allMenge = IPV1 allMenge
    istMenge (IPV1 m) = istMenge m
    vereinige (IPV1 m1) (IPV1 m2) = IPV1 $ vereinige m1 m2
    schneide (IPV1 m1) (IPV1 m2) = IPV1 $ schneide m1 m2
    zieheab (IPV1 m1) (IPV1 m2) = IPV1 $ zieheab m1 m2
    istTeilmenge (IPV1 m1) (IPV1 m2) = istTeilmenge m1 m2
    zeige (IPV1 m) = zeige m



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
