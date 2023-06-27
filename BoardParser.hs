module BoardParser
where
import Text.Parsec
import Text.Parsec.String (Parser)
import GHC.Read (readField)

import System.IO
import Data.Char (digitToInt)
import Data.Text.Internal.Builder.Int.Digits (digits)
import GameState 
import IksOks

playerParser :: Parsec String () Igrac
playerParser =
  (mojIks >> return X)
    <|> (mojOks >> return O)
    <|> (spaces >> return Prazno)


mojIks :: Parsec String () Char
mojIks = do
    spaces
    iks <- char 'X'
    spaces
    return iks
mojOks :: Parsec String () Char
mojOks = spaces >> char 'O' >>= \oks -> spaces >> return oks

separator :: Parsec String () ()
separator = spaces >> char '|' >> spaces
{-
izbrisiNewLines :: String -> String
izbrisiNewLines (x:xs) = case x     
-}
sepMoja :: Parsec String () [Igrac]
sepMoja = do
    separator
    endBy playerParser separator


napravuTabluMaybe :: String -> Tabla Igrac
napravuTabluMaybe string = case parse sepMoja "" string of
    Left _ -> Tabla [[]]
    Right values -> makeIksOksTableFromChars values

probaString :: String
probaString = "| X | O | P |\n| P | P | P |\n| O | O | P |\n"

ukloniNewLine :: String -> String
ukloniNewLine "" = ""
ukloniNewLine (x:x1:xs) = if x == '\n' then ukloniNewLine xs else x: ukloniNewLine (x1:xs)
--------------------------------------------------------------------------
-- sad parsiranje za (0, 0) itd itd, a mozda da probas da li ce da radi sa fajlom?
parsePolje :: Parsec String () Igrac
parsePolje = do
            --spaces
            --char '|'
            --spaces
            polje <- char 'X' <|> char 'O'  <|> char 'P' -- option ' ' (char 'X' <|> char 'O') 
            --spaces -- SPACES bi trebalo da cita sva prazna polja ukljucujuci i \n, \t, \r ... 
            --char '|'
            case polje of
                'X' -> return X
                'O' -> return O
                _ -> return Prazno

-- ParseTabla -> sepBy parseRow (separator '|')
-- spaces >> char '|' >> spaces tj. separator
-- >> many $ sepBy parsePolje (separator '|')

-- OKKK ovo radi, samo provali kako da umesto ovako ruzno da ispises da koristis neki many ili tk nest??? 
-- > parser proba "" probaString 
-- !!! nemoj koristiti jer je za 3 reda samo, ali neka stoji
myParser :: Parsec String () [Igrac]
myParser = do
            separator
            prviRed <- endBy parsePolje separator

            separator
            drugiRed <- endBy parsePolje separator

            separator
            treciRed <- endBy parsePolje separator
            return $ prviRed ++ drugiRed ++ treciRed


-- cita ovo: | X | O | P |
parseRow :: Parsec String () [Igrac]
parseRow = do
            separator --separator cita sve white spaces
            endBy parsePolje separator

-- cita sve redove, koliko god da ih ima! zato je bolja opcija od ovog myParser
parseTable :: Parsec String () [Igrac]
parseTable = do
        matrix <- endBy parseRow spaces
        return $ concat matrix
-----------------------------------------------------------
leftBracket :: Parsec String () ()
leftBracket = spaces >> char '(' >> spaces

rightBracket :: Parsec String () ()
rightBracket = spaces >> char ')' >> spaces

comma :: Parsec String () ()
comma = spaces >> char ',' >> spaces

parseCoordinates :: Parsec String () (Int, Int)
parseCoordinates = do
                    spaces
                    firstCoordinate <- many1 digit
                    comma
                    secondCoordinate <- many1 digit
                    return (read firstCoordinate, read secondCoordinate)

-- parse parseMove "" "(0, 0)"
parseMove :: Parsec String () (Int, Int)
parseMove = between leftBracket rightBracket parseCoordinates

parseMoves :: Parsec String () [(Int, Int)]
parseMoves = endBy parseMove spaces

parseFile :: Parsec String () (Tabla Igrac, [(Int, Int)])
parseFile = do
                igraci <- parseTable
                potezi <- parseMoves
                let table = makeIksOksTableFromChars igraci

                return (table, potezi)

-----------------------------------------------------------
main :: IO ()
main = do
    contents <- readFile "input.txt"

    case parse parseFile "" contents of
        Left err -> print err
        Right val -> print $ runStateHistory (applyMovesH (snd val)) (fst val)


--print $ runStateHistory (applyMovesH (snd val)) (fst val)

