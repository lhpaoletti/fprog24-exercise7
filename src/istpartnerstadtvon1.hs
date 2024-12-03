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
    allMenge =
        let cities = [minBound..maxBound] :: [Landeshauptstadt]
        in cross (MT1 cities) (MT1 cities)
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
