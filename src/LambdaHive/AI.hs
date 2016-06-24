{-# LANGUAGE OverloadedStrings #-}

module LambdaHive.AI where

import           AI.Minimax
import qualified Data.Bimap       as Bimap
import           Data.Bool
import qualified Data.IGraph      as IG
import           Data.List
import qualified Data.Map.Strict  as Map
import           Data.Maybe
import           Data.Monoid
import qualified Data.Set         as Set
import           Data.Time.Clock
import           LambdaHive.Types
import           System.Random

data HiveAI = RandomAI | Minimax ScoreWeights Int (ScoreWeights -> GameState -> Value)
instance Eq HiveAI where
  (==) RandomAI RandomAI = True
  (==) Minimax{} Minimax{} = True
  (==) _ _ = False

instance Show HiveAI where
  show RandomAI = "RandomAI"
  show (Minimax _ d _) = "Minimax - depth: " ++ show d

aiMove :: HiveAI -> GameState -> IO (HiveMove, GameState)
aiMove ai gs
   | null allMoves = return (NoOp,unsafeSkipTurn gs)
   | otherwise = go ai
   where
      allMoves = validPlayerMoves gs
      go RandomAI = do
        r <- randomRIO (0, length allMoves -1)
        let mv = allMoves !! r
        putStrLn "Random Move"
        return (mv, fromJust $ makeMove gs mv)
      go (Minimax sw d s) = do
        t <- getCurrentTime
        let (mv, score) = searchMove (alphaBeta (s sw)) d gs
        print score
        t2 <- getCurrentTime
        print $ diffUTCTime t2 t
        return (mv, fromJust $ makeMove gs mv)

data ScoreWeights = ScoreWeights
  { swPlayerArtPoints   :: Int
  , swOppArtPoints      :: Int
  , swOppQBreathing     :: Int
  , swPlayerQBreathing  :: Int
  , swPiecesToWin       :: Int
  , swPlayerPiecesOnTop:: Int
  , swOppPiecesOnTop    :: Int
  }
  deriving (Show, Eq, Read)

score1 :: ScoreWeights -> GameState -> Value
score1 sw gs = case prog of
  Win wp -> if wp == p then maxBound-1 else minBound+1
  Draw -> 0
  InProgress -> Value $
              (swPlayerArtPoints sw * (-1) * getPieceWeightSum playerArtPoints)
              + (swOppArtPoints sw * getPieceWeightSum oposingArtPoints)
              + (swOppQBreathing sw * fromMaybe 0 ((-) 6 <$> opposingQueenBreathingRoom))
              + (swPlayerQBreathing sw * (-1) * fromMaybe 0 ((-) 6 . breathingRoom gs <$> queen p))
              + (swPiecesToWin sw * fromMaybe 0 (bool (-1) 1 <$> enoughPiecesToWin))
              + (swPlayerPiecesOnTop sw * pieceOnTopSum gs p)
              + (swOppPiecesOnTop sw * pieceOnTopSum gs oposingPlayer)
  where
    p = gsCurrPlayer gs
    prog = gsStatus gs
    bs =  gsBoard gs
    idMap = bsIdMap bs
    coords = bsCoords bs
    oposingPlayer
      | p == Player1 = Player2
      | otherwise = Player1
    queen pl = (Bimap.!>) (bsIdMap bs) <$> maybeQueen pl
    maybeQueen pl = Bimap.lookup (playerText pl <> "Q") $ bsCannonicalIdMap bs
    artPoints = map (\pId -> coords Map.! (idMap Bimap.! pId)) $ Set.toList $ bsArticulationPoints bs
    playerArtPoints = filter (\piece -> hPlayer piece == p) artPoints
    oposingArtPoints = filter (\piece -> hPlayer piece /= p) artPoints
    opposingQueenBreathingRoom = breathingRoom gs <$> queen oposingPlayer
    enoughPiecesToWin = (>=) (piecesFreeToAttck gs p) <$> opposingQueenBreathingRoom
    getPieceWeightSum = sum . map (pieceWeight . hPieceType)

pieceWeight :: PieceType -> Int
pieceWeight Spider = 1
pieceWeight Beetle = 3
pieceWeight Grasshopper = 2
pieceWeight Ant = 3
pieceWeight Queen = 6

mapSum :: (Num a) => (k -> b -> a) -> Map.Map k b -> a
mapSum f = Map.foldlWithKey' (\s k v -> s + f k v) 0

pieceOnTopSum :: GameState -> HivePlayer -> Int
pieceOnTopSum gs p = sumWeightOfTrapped $ Map.filterWithKey onTopPiece coords
  where
    bs = gsBoard gs
    coords = bsCoords bs
    sumWeightOfTrapped = mapSum (\pc _ -> mapSum (\ _ p2 -> pieceScore p2) (othersInStack pc))
    pieceScore p1 = pieceWeight (hPieceType p1) * bool 10 1 (hPlayer p1 == p)
    othersInStack pc = Map.filterWithKey (axialEqNotSame pc) coords
    axialEqNotSame pc@(_,_,h) pc1@(_,_,h2) _ = axialEq pc pc1 && h /= h2
    onTopPiece pc@(_,_,h) p1 = hPlayer p1 == p
                    && h > 0
                    && topOfTheStack bs pc

piecesFreeToAttck :: GameState -> HivePlayer -> Int
piecesFreeToAttck gs p = Map.size $ Map.filterWithKey freeToAttack coords
  where
    bs = gsBoard gs
    coords = bsCoords bs
    adjacency = bsAdjacency bs
    idMap = bsIdMap bs
    connMap = bsCannonicalIdMap bs
    queenText = playerText oposingPlayer <> "Q"
    queenExists = Bimap.member queenText connMap
    queenId =  idMap Bimap.!> (connMap Bimap.! queenText)
    adjacentToQueen = if queenExists
                      then IG.neighbours queenId adjacency
                      else []
    oposingPlayer
      | p == Player1 = Player2
      | otherwise = Player1
    freeToAttack pc pl = (hPlayer pl == p)
                       && (hPieceId pl `notElem` adjacentToQueen)
                       && not (null $ validPieceMoves gs pc)

breathingRoom :: GameState -> PieceId -> Int
breathingRoom gs pId = 6 - Set.size intersect
  where
    bs = gsBoard gs
    adjacency = bsAdjacency bs
    heights = Map.keysSet $ bsStackHeights bs
    pc = bsIdMap bs Bimap.! pId
    intersect = Set.intersection neighbors heights
    neighbors = Set.fromList $ map ((\(x,y,_) -> (x,y)) . getNeighbor pc) allDirections
