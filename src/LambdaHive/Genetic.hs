module LambdaHive.Genetic where

import           Control.Monad
import qualified Control.Monad.Parallel as MP
import           Data.List
import           Data.List.Split
import           LambdaHive.AI
import           LambdaHive.Types
import           System.IO
import           System.Random

data GeneticGameResult = GeneticWinner HivePlayer | GeneticDraw | OverTurnLimit
  deriving (Eq, Ord, Show)

tryRandomBeaters :: IO ()
tryRandomBeaters = do
  gameLog <- openFile "genetic-weights2.txt" ReadMode
  weights <- splitOn "\n" <$> hGetContents gameLog
  let matchups =  zip (repeat RandomAI)
                  $ map ((\w -> Minimax w 1 score1) . read)
                  $ filter (/= "") weights
  nextRound <- doRound 200 matchups
  gLog2 <- openFile "genetic-weights3.txt" WriteMode
  mapM_ (\(a,b) -> do
    hPutStrLn gLog2 (weightString a)
    hPutStrLn gLog2 (weightString b)
    ) nextRound
  hFlush gameLog
  where
    weightString RandomAI = "Random wins"
    weightString (Minimax sw _ _) = show sw

getRandomBeaters :: IO ()
getRandomBeaters = do
  ws <- map (\w -> Minimax w 3 score1) <$> replicateM 5 getRandomScoreWeights
  let matchups = zip ws (repeat RandomAI)
  nextRound <- doRound 50 matchups
  gameLog <- openFile "genetic-weights.txt" WriteMode
  mapM_ (\(a,b) -> do
    hPutStrLn gameLog (weightString a)
    hPutStrLn gameLog (weightString b)
    ) nextRound
  hFlush gameLog
  where
    weightString RandomAI = "Random wins"
    weightString (Minimax sw _ _) = show sw

doRound :: TurnNumber -> [(HiveAI, HiveAI)] -> IO [(HiveAI, HiveAI)]
doRound tn games = do
  nGames <- MP.mapM (uncurry $ getGameWinner tn) games
  let halfLength = floor $ genericLength nGames / (2 :: Double)
  let firstHalf = take halfLength nGames
  let secondHalf = drop halfLength nGames
  return $ zip firstHalf secondHalf

getRandomScoreWeights :: IO ScoreWeights
getRandomScoreWeights = do
  [s1,s2,s3,s4,s5,s6,s7] <- replicateM 7 (randomRIO (0,100))
  return $ ScoreWeights s1 s2 s3 s4 s5 s6 s7

playGame :: TurnNumber -> HiveAI -> HiveAI -> IO GeneticGameResult
playGame depth a1 a2 = go initGS
  where
    go gs = if gsTurn gs > depth
            then return OverTurnLimit
            else winner gs $ gsStatus gs
    winner _ Draw = return GeneticDraw
    winner _ (Win p) = return $ GeneticWinner p
    winner gs InProgress = do
      (_,nGs) <- aiMove (currAi gs) gs
      go nGs
    currAi gs
      | gsCurrPlayer gs == Player1 = a1
      | otherwise = a2

getGameWinner :: TurnNumber -> HiveAI -> HiveAI -> IO HiveAI
getGameWinner tn a1 a2 = do
  gr <- playGame tn a1 a2
  winner gr
  where
    winner (GeneticWinner Player1) = return a1
    winner (GeneticWinner Player2) = return a2
    winner _
      | a1 == RandomAI = return a1
      | a2 == RandomAI = return a2
      | otherwise = do
        r <- randomRIO (0, 1) :: IO Int
        return $ if r == 0 then a1 else a2
