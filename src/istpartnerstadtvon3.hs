-- IstPartnerStadtVon1 Typ Datei
-- Datum: 06.12.
module IPV3 where
import Landeshauptstadt
import Menge
import Relation
import MT3
import Defaultable


data IstPartnerstadtVon3 = IPV3 (MT3 Staedtepaar)

instance Menge (MT3 Staedtepaar) where
    leereMenge   = MT3 (\_ -> False)
    allMenge     = MT3 (\_ ->  True)
    istMenge     = \_ -> True
    vereinige    = vereinigeMT3
    schneide     = schneideMT3
    zieheab      = zieheabMT3
    istTeilmenge = istTeilmengeMT3
    zeige        = zeigeMT3
