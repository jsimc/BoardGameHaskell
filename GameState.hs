
module GameState (
    GameState (runState),
    GameStateHistory (runStateHistory)
)
where
import OtherGameState (Tabla (..), Igrac (..), mojaTabla, proveriZavrsnoStanje, odigrajPotez, koJeNaPotezu)

newtype GameState s a = GameState {runState :: s -> (a, s)}

instance Functor (GameState s) where
  fmap f (GameState g) = GameState $ \s -> let (a, s') = g s in (f a, s')

instance Applicative (GameState s) where
  pure a = GameState $ \s -> (a, s)
  GameState f <*> GameState g = GameState $ \s ->
    let
        (fa, s') = f s
        (ga, s'') = g s'
    in (fa ga, s'')

instance Monad (GameState s) where
    return = pure
    (>>=) :: GameState s a -> (a -> GameState s b) -> GameState s b
    (GameState h) >>= f = GameState $ \s ->
        let
            (a, newState) = h s
            (GameState g) = f a
        in g newState

-- Overwrites the existing state
put :: s -> GameState s ()
put s = GameState $ \_ -> ((), s)

evalGameState :: GameState s a -> s -> a
evalGameState act = fst . runState act

execGameState :: GameState s a -> s -> s
execGameState act = snd . runState act
-------------------------------------------------------------------------------------
newtype GameStateHistory s a = GameStateHistory {runStateHistory :: s -> (a, s)}
instance (Monoid s) => Functor (GameStateHistory s) where
    fmap f (GameStateHistory g) = GameStateHistory $ \s ->
        let (a, s') = g s
        in (f a, s')

instance (Monoid s) => Applicative (GameStateHistory s) where
  pure a = GameStateHistory $ \s -> (a, s)
  (GameStateHistory f) <*> (GameStateHistory g) = GameStateHistory $ \s ->
    let
        (a1, s1) = f s
        (a2, s2) = g s1
    in (a1 a2, s2)

-- Da li ovo ima smisla??? 
instance (Monoid s) => Monad (GameStateHistory s) where
    return = pure
    (GameStateHistory g) >>= f = GameStateHistory $ \s ->
        let
            (a, s1) = g s
            (GameStateHistory newGSH) = f a
        in newGSH s1
-------------------------------------------------------------------------
applyMove :: (Int, Int) -> GameState (Tabla Igrac) Bool
applyMove pozicija =  GameState $ \tabla -> let novaTabla = odigrajPotez tabla pozicija (koJeNaPotezu tabla)
                                        in (proveriZavrsnoStanje novaTabla, novaTabla)

applyMoveH :: (Int, Int) -> GameStateHistory [Tabla Igrac] Bool
applyMoveH pozicija = GameStateHistory $ \(tabla:table)-> let novaTabla = odigrajPotez tabla pozicija (koJeNaPotezu tabla)
                                        in (proveriZavrsnoStanje novaTabla, novaTabla:tabla:table)

-- ZASTO JE X BOOL A NE GAMESTATE?????????????
-- Treba mi da mi ovo a u GameState bude zapravo tabla (i opciono moze True/False da li je zavrseno)

kvaziMain :: (Bool, Tabla Igrac)
kvaziMain = runState applyMoves mojaTabla

applyMoves :: GameState (Tabla Igrac) Bool
applyMoves = do
    applyMove (1, 1)
    applyMove (2, 0)
    applyMove (2, 2)
    applyMove (0, 2)
    applyMove (0, 0)

initialize :: GameStateHistory (Tabla Igrac) Bool
initialize = GameStateHistory $ \xs -> (proveriZavrsnoStanje xs, xs)

applyMovesH :: GameStateHistory [Tabla Igrac] Bool
applyMovesH = do
   -- initialize
    applyMoveH (1, 1)
    applyMoveH (0, 0)
    applyMoveH (2, 2)

kvaziMainH :: (Bool, [Tabla Igrac])
kvaziMainH = runStateHistory applyMovesH [mojaTabla]
{-
-- Definisem vrednosti za tablu
newtype Field a = Field a
newtype Row a = Row [Field a] 
newtype BoardGame a = BoardGame [Row a] 
-- First == X, Second == O
data Player = X | O deriving (Show)
-- Player je koji igrac je odigrao potez, a (Int, Int) predstavlja (row, column) polja gde je odigran potez
data Move a = Move {currentPlayer :: Player, currentField :: Field a, currentPosition :: (Int, Int)}

instance Show a => Show (Field a) where
    show (Field a) = show a

instance Show a => Show (Row a) where
    show (Row fields) = concatMap (\field -> "|"++show field) fields ++ "|\n"

instance Show a => Show (BoardGame a) where
    show (BoardGame rows) = concatMap show rows

-- Function to toggle the player
togglePlayer :: Player -> Player
togglePlayer X = O
togglePlayer O = X


-- Ovo mi deluje kao da ima smisla da je Field funktor? Ne bi trebalo da smeta.. Pa nek stoji
instance Functor Field where
    fmap f (Field a) = Field (f a) 


myBoard :: BoardGame Int
myBoard = BoardGame [Row [Field 1, Field 2],
                Row [Field 3, Field 4]]

getRow :: BoardGame a -> Int -> Row a
getRow (BoardGame rows) num = rows !! num

getFieldFromRow :: Row a -> Int -> Field a
getFieldFromRow (Row fields) num = fields !! num

-- board, row, column
getField :: BoardGame a -> Int -> Int -> Field a
getField (BoardGame rows) row = getFieldFromRow (rows !! row)


playMove :: BoardGame a -> Move a
playMove board = Move X (getField board 0 1) (0, 1)
--------------------------------------------------------------
-- Starting point for iks oks
-- Zasto nije htelo u drugom fajlu da se ucita ovako??? Pitaj Ciganovica sjutra
iksOksBoard :: BoardGame Char
iksOksBoard = BoardGame [   Row [Field '_', Field '_', Field '_'],
                            Row [Field '_', Field '_', Field '_'],
                            Row [Field '_', Field '_', Field '_']]

-- Current board state 
validMoves :: BoardGame a -> [(Int, Int)]
validMoves (BoardGame [Row a]) = let rows
-}
