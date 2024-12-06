-- IstPartnerStadtVon1 Typ Datei
-- Datum: 02.12.
module IPV1 where
import Landeshauptstadt
import Menge
import Relation
import MT1


data IstPartnerstadtVon1 = IPV1 (MT1 Staedtepaar)

instance Menge IstPartnerstadtVon1 where
    leereMenge = IPV1 leereMenge
    allMenge   = IPV1 allMenge
    istMenge (IPV1 m) = istMenge m
    zeige    (IPV1 m) =    zeige m
    vereinige    (IPV1 m1) (IPV1 m2) = IPV1 $ vereinige m1 m2
    schneide     (IPV1 m1) (IPV1 m2) = IPV1 $  schneide m1 m2
    zieheab      (IPV1 m1) (IPV1 m2) = IPV1 $   zieheab m1 m2
    istTeilmenge (IPV1 m1) (IPV1 m2) =     istTeilmenge m1 m2

instance Relation IstPartnerstadtVon1 where
    istLinkstotal  (IPV1 r) = istLinkstotal  r
    istRechtstotal (IPV1 r) = istRechtstotal r
    istReflexiv    (IPV1 r) = istReflexiv    r
    istSymmetrisch (IPV1 r) = istSymmetrisch r
    istTransitiv   (IPV1 r) = istTransitiv   r
