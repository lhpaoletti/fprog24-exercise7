-- IstPartnerStadtVon1 Typ Datei
-- Datum: 06.12.
module IPV3 where
import Landeshauptstadt
import Menge
import Relation
import MT3


data IstPartnerstadtVon3 = IPV3 (MT3 Staedtepaar)

instance Menge IstPartnerstadtVon3 where
    leereMenge = IPV3 leereMenge
    allMenge = IPV3 allMenge
    istMenge (IPV3 m) = istMenge m
    vereinige (IPV3 m1) (IPV3 m2) = IPV3 $ vereinige m1 m2
    schneide (IPV3 m1) (IPV3 m2) = IPV3 $ schneide m1 m2
    zieheab (IPV3 m1) (IPV3 m2) = IPV3 $ zieheab m1 m2
    istTeilmenge (IPV3 m1) (IPV3 m2) = istTeilmenge m1 m2
    zeige (IPV3 m) = zeige m

instance Relation IstPartnerstadtVon3 where
    istLinkstotal  (IPV3 r) = istLinkstotal  r
    istRechtstotal (IPV3 r) = istRechtstotal r
    istReflexiv    (IPV3 r) = istReflexiv    r
    istSymmetrisch (IPV3 r) = istSymmetrisch r
    istTransitiv   (IPV3 r) = istTransitiv   r
