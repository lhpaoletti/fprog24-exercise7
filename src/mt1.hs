-- MT1 Typ Datei
-- Datum: 29.11.
module MT1 where
import Menge
import Defaultable
import Landeshauptstadt


newtype MT1 e = MT1 [e] 


instance Menge (MT1 Char) where
    leereMenge   = MT1 []
    allMenge     = MT1 defaultValue
    istMenge     = istMengeMT1
    vereinige    = vereinigeMT1
    schneide     = schneideMT1
    zieheab      = zieheabMT1
    istTeilmenge = istTeilmengeMT1
    zeige        = zeigeMT1

instance Menge (MT1 Int) where
    leereMenge   = MT1 []
    allMenge     = MT1 defaultValue
    istMenge     = istMengeMT1
    vereinige    = vereinigeMT1
    schneide     = schneideMT1
    zieheab      = zieheabMT1
    istTeilmenge = istTeilmengeMT1
    zeige        = zeigeMT1

instance Menge (MT1 Landeshauptstadt) where
    leereMenge   = MT1 []
    allMenge     = MT1 defaultValue
    istMenge     = istMengeMT1
    vereinige    = vereinigeMT1
    schneide     = schneideMT1
    zieheab      = zieheabMT1
    istTeilmenge = istTeilmengeMT1
    zeige        = zeigeMT1



istMengeMT1 :: Eq e => MT1 e -> Bool
istMengeMT1 (MT1     []) = True
istMengeMT1 (MT1 (e:es)) =
    all (/= e) es && (istMengeMT1 . MT1) es

vereinigeMT1 :: Eq e => MT1 e -> MT1 e -> MT1 e
vereinigeMT1 m1@(MT1 list1) m2@(MT1 list2)
      | istMengeMT1 m1 && istMengeMT1 m2 = MT1 . nub $ list1 ++ list2
      | otherwise                        = Menge.fehlermeldung

schneideMT1 :: Eq e => MT1 e -> MT1 e -> MT1 e
schneideMT1 m1@(MT1 list1) m2@(MT1 list2)
      | istMengeMT1 m1 && istMengeMT1 m2 = MT1 $ [e | e <- list1, e `elem` list2]
      | otherwise                        = Menge.fehlermeldung

zieheabMT1 :: Eq e => MT1 e -> MT1 e -> MT1 e
zieheabMT1 m1@(MT1 list1) m2@(MT1 list2)
      | istMengeMT1 m1 && istMengeMT1 m2 = MT1 $ [e | e <- list1, e `notElem` list2]
      | otherwise                        = Menge.fehlermeldung


istTeilmengeMT1 :: Eq e => MT1 e -> MT1 e -> Bool
istTeilmengeMT1 m1@(MT1 list1) m2@(MT1 list2)
      | istMengeMT1 m1 && istMengeMT1 m2 = all (`elem` list2) list1
      | otherwise                        = Menge.fehlermeldung

zeigeMT1 :: Show e => MT1 e -> MengeAlsZeichenreihe
zeigeMT1 (MT1 elems) = "{" ++ Menge.formatElems elems ++ "}"



-- Entferne Duplikate einer Liste.
nub :: Eq a => [a] -> [a]
nub     [] = []
nub (e:es) = e : (nub $ filter (/= e) es)



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
