module Data.SGF.Hive
  ( getHiveGame
  , HiveSGFGame
  , Player
  )
where

import           Data.List.Utils
import           Text.Trifecta

data Player = White | Black
  deriving (Show, Eq, Ord)

type Move = String
type Winner = Player

data HiveSGFGame = HiveSGFGame Winner [Move]
  deriving (Show, Eq, Ord)

getHiveGame :: String -> IO HiveSGFGame
getHiveGame s =  do
  test <- parseFromFile parseSGF s
  case test of
    Nothing -> return $ HiveSGFGame White []
    Just ts -> return $ HiveSGFGame White (filter (/= "") ts)

parseSGF :: Parser [Move]
parseSGF = some (choice [try hiveMove, const "" <$> skipLine])

-- test1 :: String
-- test1 ="; P0[2 dropb Q N 13 .]"
-- test2 :: String
-- test2 ="; P0[7 dropb S1 M 12 /wQ]"
-- test3 :: String
-- test3 ="; P1[4 move B G1 O 13 wQ-]"

hiveMove :: Parser Move
hiveMove = p <$> playerParser <*> between (char '[') (char ']') moveParser
  where
    p :: Char -> String -> Move
    p '0' d = if startswith "w" d then backslashfix d else "w" ++ backslashfix d
    p '1' d = if startswith "b" d then backslashfix d else "b" ++ backslashfix d
    p _ _ =  error "Can't get here"
    backslashfix = replace "\\\\" "\\"
    playerParser = string "; P" *> choice [char '0', char '1']
    moveParser :: Parser String
    moveParser = token (some digit) *> m
    m :: Parser String
    m  = s <$> choice [symbol "dropb",symbol "move B", symbol "movedone B"] <*> token (some alphaNum) <*> token (some alphaNum) <*> token (some alphaNum) <*> some (choice [alphaNum, oneOf "\\/-."])
    s :: String -> String -> String -> String -> String -> String
    s _ p1 _ _ p2 = p1 ++ " " ++ p2


skipLine :: Parser String
skipLine = manyTill anyChar endOfLine

endOfLine :: Parser Char
endOfLine = choice [newline,char '\r' *> newline]
