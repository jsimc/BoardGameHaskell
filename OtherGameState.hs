module OtherGameState (
    Tabla(Tabla),
    Polje,
    Igrac (X, O, Prazno),
    proveriZavrsnoStanje,
    odigrajPotez,
    koJeNaPotezu,
    mojaTabla
)
where
import Data.List ( transpose )
import RoseTree (Rose(..))
-- import GameState (GameState (..), GameStateHistory(..))

newtype Tabla a = Tabla [[Polje a]] -- Mozda da ubacis umesto Polje a, da bude Polje (Maybe a)
data Polje a = Polje {pozicija :: (Int, Int), vrednost :: a} -- problem je jer onda ce vrednost a da bude Nothing ili Just a, pa nece lepo pisati
data Igrac = X | O | Prazno deriving Eq


mojaTabla :: Tabla Igrac
mojaTabla = Tabla [ [Polje (0, 0) Prazno, Polje (0, 1) Prazno, Polje (0, 2) Prazno],
                    [Polje (1, 0) Prazno, Polje (1, 1) Prazno, Polje (1, 2) Prazno],
                    [Polje (2, 0) Prazno, Polje (2, 1) Prazno, Polje (2, 2) Prazno]]

instance Show a => Show (Polje a) where
    show (Polje (row, col) x) = show x

instance Show Igrac where
    show X = "X"
    show O = "O"
    show Prazno = " "

instance Show a => Show (Tabla a) where
    show (Tabla rows) = "\n" ++ unlines (map showRow rows)
        where
        showRow row = unwords (map (("| "++) . show) row) ++ " |"

iksOksIndeksi :: [(Int, Int)]
iksOksIndeksi = [(row, col) | row <- [0, 1, 2], col <- [0, 1, 2]]


-- Polje je zapravo --> \polje vrednost -> Polje polje vrednost,
-- iksOksIndeksi su [(Int, Int)], a [Char] je argument koji se prihvata
-- posto su indeksi uvek isti njih ne moram kao argument prosledjivati
makePolja :: [Igrac] -> [Polje Igrac]
makePolja = zipWith Polje iksOksIndeksi

makeIksOksTableTmp :: Int -> [a] -> [[a]]
makeIksOksTableTmp _ [] = []
makeIksOksTableTmp cols xs = row : makeIksOksTableTmp cols remaining
  where
    (row, remaining) = splitAt cols xs

mergePolje :: Polje a -> Polje a -> Polje a
mergePolje p1 p2 = Polje (pozicija p1) (vrednost p2)

-- Uzima listu karaktera, vrednosti polja i vraca tabelu sa poljima
---------------------------------------------------------------
makeIksOksTableFromChars :: [Igrac] -> Tabla Igrac
makeIksOksTableFromChars list
    | length list == 9 = Tabla (makeIksOksTableTmp 3 (makePolja list))
    | otherwise = Tabla [[]]
---------------------------------------------------------------
makeIksOksTableFromPolje :: [Polje Igrac] -> Tabla Igrac
makeIksOksTableFromPolje polja
    | length polja == 9 = Tabla (makeIksOksTableTmp 3 polja)
    | otherwise = Tabla [[]]
---------------------------------------------------------------

concatTable :: Tabla a -> [Polje a]
concatTable (Tabla rows) = concat rows

brojPoteza :: Tabla Igrac -> Int
brojPoteza tabla = foldl (\acc field -> if vrednost field == Prazno then acc else acc+1) 0 (concatTable tabla)

koJeNaPotezu :: Tabla Igrac -> Igrac
koJeNaPotezu tabla
    | even (brojPoteza tabla) = X
    | otherwise = O

-- Okej. Treba mi informacija o indexu u tabeli, znaci ne moze samo [[a]], nego neki [[Field a]]
-- Koji ce da cuva (row, col) informaciju u sebi, plus ce da cuva val, znaci on moze biti (val, (row, col))

validniPotezi :: Tabla Igrac -> [(Int, Int)]
validniPotezi (Tabla rows) = [pozicija field | row <- rows, field <- row, vrednost field == Prazno]

-- Ja bih napravila novu funkciju ValidniPotezi gde ce da vraca niz tabli kako izgledaju nakon poteza.
-- E sad trebace mi onaj togglePlayer za ubacivanje u rose strukturu, trebace mi i brojac da znamo na kom smo nivou u drvetu? 

odigrajPotez1 :: [Polje Igrac] -> (Int, Int) -> Igrac -> [Polje Igrac]
odigrajPotez1 [] _ _ = []
odigrajPotez1 (x:xs) pozicija1 igrac = if pozicija x == pozicija1 then Polje (pozicija x) igrac : xs else x : odigrajPotez1 xs pozicija1 igrac
---- ODJE PROBLEM KAKO GLEDATI DA ZAVRSNO STANJE BUDE LEAF NODE
odigrajPotez :: Tabla Igrac -> (Int, Int) -> Igrac -> Tabla Igrac
odigrajPotez tabla pozicija1 igrac = makeIksOksTableFromPolje (odigrajPotez1 (concatTable tabla) pozicija1 igrac)
---- 
-- za datu tablu vraca niz sledecih tabli
-- Koji igrac je na potezu izracunace pomocu fje koJeNaPotezu
validniPoteziTable :: Tabla Igrac -> [Tabla Igrac]
validniPoteziTable tabla = map (\pozicija -> odigrajPotez tabla pozicija (koJeNaPotezu tabla)) (validniPotezi tabla)

popunjenaTablaTmp :: [Polje Igrac] -> Bool
popunjenaTablaTmp (x:xs)
    | vrednost x == Prazno = False
    | otherwise = popunjenaTablaTmp xs
popunjenaTablaTmp [] = True

popunjenaTabla :: Tabla Igrac -> Bool
popunjenaTabla tabla = popunjenaTablaTmp $ concatTable tabla

proveriNiz :: [Polje Igrac] -> Bool
proveriNiz [Polje _ Prazno, Polje _ Prazno, Polje _ Prazno] = False
proveriNiz [p1, p2, p3] = vrednost p1 == vrednost p2 && vrednost p2 == vrednost p3
proveriNiz _ = False

proveriDijagonalu :: Tabla Igrac -> Bool
proveriDijagonalu (Tabla tabla) = let
    glavnaDijagonala = map (\i -> tabla !! i !! i) [0, 1, 2]
    sporednaDijagonala = map (\i -> tabla !! i !! (2-i)) [0, 1, 2]
    in proveriNiz glavnaDijagonala || proveriNiz sporednaDijagonala

-- Verovatno bih trebala odvojiti slucaj kad je pobeda i kad nije pobeda
proveriZavrsnoStanje :: Tabla Igrac -> Bool
proveriZavrsnoStanje (Tabla tabla) =
    any proveriNiz tabla ||
    any proveriNiz (transpose tabla) ||
    proveriDijagonalu (Tabla tabla) ||
    popunjenaTabla (Tabla tabla)
------------------------------------------------------------------------
stabloPoteza :: Tabla Igrac -> Rose (Tabla Igrac)
stabloPoteza tabla
    | proveriZavrsnoStanje tabla = Node tabla []
    | otherwise = Node tabla (map stabloPoteza (validniPoteziTable tabla))
------------------------------------------------------------------------
{-
applyMove :: Tabla Igrac -> (Int, Int) -> GameState (Tabla Igrac) Bool
applyMove (Tabla tabla) pozicija = GameState ( const
  (proveriZavrsnoStanje (Tabla tabla),
   odigrajPotez pozicija (koJeNaPotezu tabla)))
-}
{-
tmp :: Tabla Igrac -> Bool
tmp (Tabla tb) = any proveriNiz tb 

tmpTranspose :: Tabla Igrac -> Tabla Igrac
tmpTranspose (Tabla tb)= Tabla (transpose tb)
-}
