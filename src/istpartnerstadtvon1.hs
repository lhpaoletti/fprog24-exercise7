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
