module LambdaHive.Types where

import           Data.Graph
import           Data.List
import           Data.Map   (Map)
import qualified Data.Map   as Map
import           Data.Maybe
import qualified Data.Set   as Set

data HivePlayer = Player1 | Player2
  deriving (Show, Eq, Ord)
data PieceType = Ant | Queen | Beetle | Grasshopper | Spider
  deriving (Show, Eq, Ord)

type PieceId = Int
--Axial coordinates with height (x,y,height) http://www.redblobgames.com/grids/hexagons/
type PieceCoordinate = (Int,Int,Int)

data HivePiece = HivePiece
  { hPlayer    :: HivePlayer
  , hPieceId   :: PieceId
  , hPieceType :: PieceType
  }
  deriving (Show, Eq, Ord)

type BoardAdjecency = Graph
type BoardState = (Map PieceCoordinate HivePiece, BoardAdjecency)
type PlayerHand = [PieceType]
type GameState = (HivePlayer,BoardState,PlayerHand,PlayerHand)

startingHand :: [PieceType]
startingHand = [Ant,Ant,Ant,Queen,Beetle,Beetle,Grasshopper,Grasshopper,Grasshopper,Spider,Spider]

emptyBS :: BoardState
emptyBS = (Map.empty, buildG (0,0) [])
testBS0 :: BoardState
testBS0 = fromJust $ placePiece emptyBS True (0,0,0) Ant Player1
testBS1 :: BoardState
testBS1 = fromJust $ placePiece testBS0 True (-1,0,0) Ant Player2
testBS2 :: BoardState
testBS2 = fromJust $ placePiece testBS1 False (1,0,0) Ant Player1
testBS3 :: BoardState
testBS3 = fromJust $ placePiece testBS2 False (-2,0,0) Ant Player2
testBS4 :: BoardState
testBS4 = fromJust $ placePiece testBS3 False (1,-1,0) Spider Player1
testBS5 :: BoardState
testBS5 = fromJust $ placePiece testBS4 False (-3,0,0) Queen Player2
circleBS :: BoardState
circleBS = fromJust $ placePiece bs5 True (1,-1,0) Queen Player1
  where bs1 = fromJust $ placePiece emptyBS True (0,-1,0) Queen Player1
        bs2 = fromJust $ placePiece bs1 True (-1,0,0) Queen Player1
        bs3 = fromJust $ placePiece bs2 True (-1,1,0) Queen Player1
        bs4 = fromJust $ placePiece bs3 True (0,1,0) Queen Player1
        bs5 = fromJust $ placePiece bs4 True (1,0,0) Queen Player1

pieceMoves :: BoardState -> PieceCoordinate -> [PieceCoordinate]
pieceMoves bs@(pcs,_) c
  | not $ topOfTheStack bs c = []
  | not $ oneHiveRuleSatisfied bs c = []
  | otherwise = pieceTypeMoves (Map.keys pcs) c (hPieceType $ pcs Map.! c)

pieceTypeMoves :: [PieceCoordinate] -> PieceCoordinate -> PieceType -> [PieceCoordinate]
pieceTypeMoves pcs c Queen = oneSpaceMove pcs Queen c
pieceTypeMoves pcs c Ant = multiSpaceMove (const False) pcs c
pieceTypeMoves pcs c Beetle = oneSpaceMove pcs Beetle c
pieceTypeMoves _ _ Grasshopper = undefined
pieceTypeMoves pcs c Spider = multiSpaceMove (== 3) pcs c

oneSpaceMove :: [PieceCoordinate] -> PieceType -> PieceCoordinate -> [PieceCoordinate]
oneSpaceMove pcs pT c = filter (canSlide pcs c)
                      $ filter (\p -> any (adjacentCoords p) (delete c pcs))
                      $ oneAway pcs c pT \\ pcs

multiSpaceMove :: (Int -> Bool) -> [PieceCoordinate] -> PieceCoordinate -> [PieceCoordinate]
multiSpaceMove f pcs c  = go 0 Set.empty [c]
  where
    go _ visited [] = Set.toList visited
    go step visited cs
      | f (step+1) = newSpots
      | otherwise = go (step+1) newVisited newSpots
      where newSpots = filter (`Set.notMember` visited)
                     $ concatMap (oneSpaceMove pcs Queen) cs
            newVisited = Set.union visited (Set.fromList newSpots)

--     >-<
--  >-< A >-<
-- < C >-< D >
--  >-< B >-<
--     >-<
-- Let's say the beetle is at B and wants to move to A. Take the beetle temporarily off of B.
-- If the shortest stack of tiles of C and D is taller than the tallest stack of tiles of A and B,
-- then the beetle can't move to A. In all other scenarios the beetle is free to move from B to A.
canSlide :: [PieceCoordinate] -> PieceCoordinate -> PieceCoordinate -> Bool
canSlide pcs (x1,y1,_) (x2,y2,_)
  | x1 == x2 && y1 > y2 = slideable [(x1-1,y1), (x1+1,y1+1)]
  | x1 == x2 && y1 < y2 = slideable [(x1+1,y1), (x1-1,y1-1)]
  | y1 == y2 && x1 > x2 = slideable [(x1,y1-1), (x1-1,y1+1)]
  | y1 == y2 && x1 < x2 = slideable [(x1,y1+1), (x1+1,y1-1)]
  | x1 > x2 && y1 < y2  = slideable [(x1-1,y1), (x1,y1+1)]
  | x1 < x2 && y1 > y2  = slideable [(x1+1,y1), (x1,y1-1)]
  | otherwise = False
  where slideable xs = minimum (map (stackHeight pcs) xs) <= maxStackHeight
        maxStackHeight = maximum [stackHeight pcs (x2,y2), stackHeight pcs (x1,y1) - 1]


stackHeight :: [PieceCoordinate] -> (Int,Int) -> Int
stackHeight pcs (x,y) = genericLength $ filter (\(x1,y1,_) -> x1 == x && y1 == y) pcs

oneAway :: [PieceCoordinate] -> PieceCoordinate -> PieceType-> [PieceCoordinate]
oneAway pcs (x,y,_) Beetle = groundLevel ++ beetleLevel
  where groundLevel = oneAway pcs (x,y,0) Queen
        beetleLevel = map (\((w,t),v) -> (w,t,v+1))
                      $ Map.toList
                      $ Map.map maximum
                      $ Map.fromList
                      $ map (\(x1,y1,h1) -> ((x1,y1),[h1]))
                      $ filter (\(x1,y1,_) ->
                        any (\(x2,y2,_) -> x2==x1 && y2==y1) pcs
                      ) groundLevel
oneAway _ (x,y,h) _ = [(x-1,y,h),(x+1,y,h),(x,y+1,h),(x,y-1,h),(x+1,y-1,h),(x-1,y+1,h)]

oneHiveRuleSatisfied :: BoardState -> PieceCoordinate -> Bool
oneHiveRuleSatisfied bs = (<= 2) . length . scc . snd . removePiece bs

topOfTheStack :: BoardState -> PieceCoordinate -> Bool
topOfTheStack (pcs,_) (x,y,h) = (== h) $ maximum
                                  $ map (\(_,_,h1) -> h1)
                                  $ filter (\(x1,y1,_) -> x==x1 && y==y1)
                                  $ Map.keys pcs

removePiece :: BoardState -> PieceCoordinate -> BoardState
removePiece (pcs,ba) c = (newMap, newGraph)
  where
    newMap = Map.delete c pcs
    removedId = hPieceId $ pcs Map.! c
    newGraph = buildG (0,Map.size pcs-1) $ filter (\(e1,e2) -> e1 /= removedId && e2 /= removedId) (edges ba)

placePiece ::  BoardState -> Bool -> PieceCoordinate -> PieceType -> HivePlayer -> Maybe BoardState
placePiece bs@(pcs, ba) firstTurn c t p
  | c `Map.member` pcs = Nothing
  | not firstTurn && wrongPlayer = Nothing
  | otherwise = Just (newMap, newGraph)
  where
    newGraph = buildG (0,Map.size pcs) $ edges ba ++ concatMap ((\i -> [(i,newPieceId),(newPieceId,i)]) . hPieceId) (Map.elems adjacents)
    newMap = Map.insert c (HivePiece p newPieceId t) pcs
    newPieceId = Map.size pcs
    wrongPlayer = (> 0) $ length $ Map.filter (\p2 -> hPlayer p2 /= p) topOfStackAdjacents
    topOfStackAdjacents = Map.filterWithKey (\k _ -> topOfTheStack bs k) adjacents
    adjacents = Map.filterWithKey (\k _ -> adjacentCoords c k) pcs

adjacentCoords :: PieceCoordinate -> PieceCoordinate -> Bool
adjacentCoords (x,y,_) (x1,y1,_)
  | x1 == x && y1 == y = True
  | x1 == x = abs (y1 - y) == 1
  | y1 == y = abs (x1 - x) == 1
  | otherwise = (y - y1 == 1 && x - x1 == -1) || (y - y1 == -1 && x -x1 == 1)
