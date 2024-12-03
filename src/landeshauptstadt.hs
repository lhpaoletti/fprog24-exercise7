-- Landeshauptstadt Typ Datei
-- Datum: 29.11.
module Landeshauptstadt where
import Defaultable
import Menge
import MT1


data Landeshauptstadt = B | E | G | I | K | L | P | S | W
    deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- Man darf nicht `deriving` fuer `type` Deklarationen
--  nutzen, weil es nur ein Typsynonym ist
type Staedtepaar = (Landeshauptstadt, Landeshauptstadt)

data Staedtepaar1 = SP1 Landeshauptstadt Landeshauptstadt
    deriving (Eq, Ord, Show, Read, Bounded)

newtype Staedtepaar2 = SP2 (Landeshauptstadt, Landeshauptstadt)
    deriving (Eq, Ord, Show, Read, Bounded)

-- Man kann nicht `deriving` fuer Typen nutzen,
--  die charakteristische Funktionen sind
data Staedtepaar3 = SP3 ((Landeshauptstadt, Landeshauptstadt) -> Bool)
data Staedtepaar4 = SP4 (Landeshauptstadt -> Landeshauptstadt -> Bool)



instance Defaultable Landeshauptstadt where
    defaultValue = [minBound .. maxBound]

instance Menge (MT1 Landeshauptstadt) where
    leereMenge   = MT1 []
    allMenge     = MT1 defaultValue
    istMenge     = istMengeMT1
    vereinige    = vereinigeMT1
    schneide     = schneideMT1
    zieheab      = zieheabMT1
    istTeilmenge = istTeilmengeMT1
    zeige        = zeigeMT1



main = do
    putStrLn $ "leereMenge: " ++ zeige (leereMenge :: MT1 Landeshauptstadt)
    putStrLn $ "allMenge  : " ++ zeige (allMenge   :: MT1 Landeshauptstadt)
    putStrLn ""
    putStrLn $ "istMenge     {}: " ++ (show $ istMenge (leereMenge :: MT1 Landeshauptstadt))
    putStrLn $ "istMenge {B, B}: " ++ (show $ istMenge $ MT1 [B, B])
    putStrLn $ "istMenge {B, E}: " ++ (show $ istMenge $ MT1 [B, E])
    putStrLn ""
    putStrLn $ "vereinige  {} {B}: " ++ (zeige . vereinige leereMenge $ MT1 [B])
    putStrLn $ "vereinige {B} {B}: " ++ (zeige $ vereinige (MT1 [B]) (MT1 [B]))
    putStrLn $ "vereinige {B} {E}: " ++ (zeige $ vereinige (MT1 [B]) (MT1 [E]))
    putStrLn ""
    putStrLn $ "schneide  {}    {B}: " ++ (zeige . schneide leereMenge $ MT1 [B])
    putStrLn $ "schneide {B}    {B}: " ++ (zeige $ schneide (MT1 [B]) (MT1    [B]))
    putStrLn $ "schneide {B} {B, E}: " ++ (zeige $ schneide (MT1 [B]) (MT1 [B, E]))
    putStrLn ""
    putStrLn $ "zieheab     {} {B}: " ++ (zeige . zieheab leereMenge $ MT1 [B])
    putStrLn $ "zieheab    {B} {B}: " ++ (zeige $ zieheab (MT1    [B]) (MT1 [B]))
    putStrLn $ "zieheab {B, E} {B}: " ++ (zeige $ zieheab (MT1 [B, E]) (MT1 [B]))
    putStrLn ""
    putStrLn $ "komplementiere . zieheab allMenge $ {B}: " ++ (zeige . komplementiere . zieheab allMenge $ MT1 [B])
    putStrLn ""
    putStrLn $ "sindGleich    {B}    {B}: " ++ (show $ sindGleich (MT1    [B]) (MT1    [B]))
    putStrLn $ "sindGleich    {B}    {E}: " ++ (show $ sindGleich (MT1    [B]) (MT1    [E]))
    putStrLn $ "sindGleich {B, E} {E, B}: " ++ (show $ sindGleich (MT1 [B, E]) (MT1 [E, B]))
    putStrLn ""
    putStrLn $ "sindUngleich    {B}    {B}: " ++ (show $ sindUngleich (MT1    [B]) (MT1    [B]))
    putStrLn $ "sindUngleich    {B}    {E}: " ++ (show $ sindUngleich (MT1    [B]) (MT1    [E]))
    putStrLn $ "sindUngleich {B, E} {E, B}: " ++ (show $ sindUngleich (MT1 [B, E]) (MT1 [E, B]))
    putStrLn ""
    putStrLn $ "istTeilmenge    {B}    {B}: " ++ (show $ istTeilmenge (MT1    [B]) (MT1    [B]))
    putStrLn $ "istTeilmenge {B, E} {E, B}: " ++ (show $ istTeilmenge (MT1 [B, E]) (MT1 [E, B]))
    putStrLn $ "istTeilmenge    {B} {B, E}: " ++ (show $ istTeilmenge (MT1    [B]) (MT1 [B, E]))
    putStrLn $ "istTeilmenge {B, E}    {B}: " ++ (show $ istTeilmenge (MT1 [B, E]) (MT1    [B]))
    putStrLn ""
    putStrLn $ "istEchteTeilmenge    {B}    {B}: " ++ (show $ istEchteTeilmenge (MT1    [B]) (MT1    [B]))
    putStrLn $ "istEchteTeilmenge {B, E} {E, B}: " ++ (show $ istEchteTeilmenge (MT1 [B, E]) (MT1 [E, B]))
    putStrLn $ "istEchteTeilmenge    {B} {B, E}: " ++ (show $ istEchteTeilmenge (MT1    [B]) (MT1 [B, E]))
    putStrLn $ "istEchteTeilmenge {B, E}    {B}: " ++ (show $ istEchteTeilmenge (MT1 [B, E]) (MT1    [B]))
    putStrLn ""
    putStrLn $ "istObermenge    {B}    {B}: " ++ (show $ istObermenge (MT1    [B]) (MT1    [B]))
    putStrLn $ "istObermenge {B, E} {E, B}: " ++ (show $ istObermenge (MT1 [B, E]) (MT1 [E, B]))
    putStrLn $ "istObermenge    {B} {B, E}: " ++ (show $ istObermenge (MT1    [B]) (MT1 [B, E]))
    putStrLn $ "istObermenge {B, E}    {B}: " ++ (show $ istObermenge (MT1 [B, E]) (MT1    [B]))
    putStrLn ""
    putStrLn $ "istEchteObermenge    {B}    {B}: " ++ (show $ istEchteObermenge (MT1    [B]) (MT1    [B]))
    putStrLn $ "istEchteObermenge {B, E} {E, B}: " ++ (show $ istEchteObermenge (MT1 [B, E]) (MT1 [E, B]))
    putStrLn $ "istEchteObermenge    {B} {B, E}: " ++ (show $ istEchteObermenge (MT1    [B]) (MT1 [B, E]))
    putStrLn $ "istEchteObermenge {B, E}    {B}: " ++ (show $ istEchteObermenge (MT1 [B, E]) (MT1    [B]))
    putStrLn ""
    putStrLn $ "sindElementeFremd {B}    {E}: " ++ (show $ sindElementeFremd (MT1 [B]) (MT1    [E]))
    putStrLn $ "sindElementeFremd {B} {B, E}: " ++ (show $ sindElementeFremd (MT1 [B]) (MT1 [B, E]))
    putStrLn ""
    putStrLn $ "sindQuerUeberlappend    {B}    {B}: " ++ (show $ sindQuerUeberlappend (MT1    [B]) (MT1    [B]))
    putStrLn $ "sindQuerUeberlappend    {B}    {E}: " ++ (show $ sindQuerUeberlappend (MT1    [B]) (MT1    [E]))
    putStrLn $ "sindQuerUeberlappend {B, G} {B, I}: " ++ (show $ sindQuerUeberlappend (MT1 [B, G]) (MT1 [B, I]))
    putStrLn $ "sindQuerUeberlappend    {B} {B, E}: " ++ (show $ sindQuerUeberlappend (MT1    [B]) (MT1 [B, E]))
    putStrLn $ "sindQuerUeberlappend {B, E}    {B}: " ++ (show $ sindQuerUeberlappend (MT1 [B, E]) (MT1    [B]))
