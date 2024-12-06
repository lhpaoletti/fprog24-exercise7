-- MT3 Typ Datei
-- Datum: 29.11.
module MT3 where
import Menge
import Defaultable
import Landeshauptstadt
import Relation


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



instance Relation (MT3 Staedtepaar) where
    istLinkstotal m@(MT3 f)
        | (not . istMenge) m = Menge.fehlermeldung
        -- Wenn fuer alle Landeshauptstaedte l, hat die Menge mindestens eine Relation (l R _)
        | otherwise = all (`elem` leftCities) defaultValue
        where leftCities = map (fst) . toList $ m

    istRechtstotal m@(MT3 f)
        | (not . istMenge) m = Menge.fehlermeldung
        -- Wenn fuer alle Landeshauptstaedte l, hat die Menge mindestens eine Relation (_ R l)
        | otherwise = all (`elem` rightCities) defaultValue
        where rightCities = map (snd) . toList $ m

    istReflexiv m@(MT3 f)
        | (not . istMenge) m = Menge.fehlermeldung
        -- Wenn fuer alle reflexive Staedtepaare, hat die Menge auch so ein Paar
        | otherwise = all (`elem` reflexiveRelations) . takeReflexivePairs $ defaultValue
        where reflexiveRelations = takeReflexivePairs . toList $ m
              takeReflexivePairs = filter (\(l, l') -> l == l')

    istSymmetrisch m
        | (not . istMenge) m = Menge.fehlermeldung
        -- Wenn fuer alle symmetrische Staedtepaare von der Menge, sind sie auch Elemente der Menge
        | otherwise = all (`elem` relations) symmetricRelations
        where relations = toList m
              symmetricRelations = map (\(l, l') -> (l', l)) relations

    istTransitiv m
        | (not . istMenge) m = Menge.fehlermeldung
        -- Wenn fuer alle Paare der Menge {(x, y) | xRa & aRy} der Schritt (=> xRy) vorgegeben ist
        | otherwise = all (`elem` relations) l_l''s
        where relations   = toList m
              rightCities = map (snd) relations
              -- Nimm alle Paare der Menge {(x, y) | xRa & aRy}
              l_l''s = [(l, l'') | (l, l'1) <- relations, (l'2, l'') <- relations, l'1 == l'2]



-- Wandle einen MT3 in einer Liste vom inneren Typ e um.
-- Sie benoetigt die Typklassebeschraenkung (Defaultable e), um sicherzustellen, 
--  dass der Typ e eine defaultValue Funktion besitzt.
-- Haette man nicht diese Beschaenkung, muesste man Bound nutzen. Es ist aber sehr
--  gross bei Int und Char.
toList :: (Defaultable e) => MT3 e -> [e]
toList (MT3 f) = filter f defaultValue
