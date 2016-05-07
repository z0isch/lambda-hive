module LambdaHive.Types where

import           Data.Graph
import           Data.List
import           Data.Map   (Map)
import qualified Data.Map   as Map
import qualified Data.Set   as Set

data HivePlayer = Player1 | Player2
  deriving (Show, Eq, Ord)

playerString :: HivePlayer -> String
playerString Player1 = "w"
playerString Player2 = "b"

data PieceType = Ant | Queen | Beetle | Grasshopper | Spider
  deriving (Show, Eq, Ord)

pieceString :: PieceType -> String
pieceString Ant = "A"
pieceString Queen = "Q"
pieceString Beetle = "B"
pieceString Grasshopper = "G"
pieceString Spider = "S"

startingHand :: [PieceType]
startingHand = [Ant,Ant,Ant,Queen,Beetle,Beetle,Grasshopper,Grasshopper,Grasshopper,Spider,Spider]

type PieceId = Int
--Axial coordinates with height (x,y,height) http://www.redblobgames.com/grids/hexagons/
type PieceCoordinate = (Int,Int,Int)

data HivePiece = HivePiece
  { hPlayer    :: HivePlayer
  , hPieceId   :: PieceId
  , hPieceType :: PieceType
  , hCannoicalId :: String
  }
  deriving (Show, Eq, Ord)

type BoardAdjecency = Graph
type BoardState = (Map PieceCoordinate HivePiece, BoardAdjecency)
type TurnNumber = Int
type PlayerHand = [PieceType]

data GameState = GameState
  { gsCurrPlayer :: HivePlayer
  , gsTurn       :: TurnNumber
  , gsBoard      :: BoardState
  , gsHand1      :: PlayerHand
  , gsHand2      :: PlayerHand
  }
  deriving (Show, Eq, Ord)

nextPlayer :: GameState -> HivePlayer
nextPlayer gs
  | gsCurrPlayer gs == Player1 = Player2
  | otherwise = Player1

currPlayersHand :: GameState -> PlayerHand
currPlayersHand gs
    | gsCurrPlayer gs == Player1 = gsHand1 gs
    | otherwise = gsHand2 gs

playedBee :: GameState -> Bool
playedBee = notElem Queen . currPlayersHand

data Neighbor = TopLeftN | LeftN | BottomLeftN | BottomRightN | RightN | TopRightN
  deriving (Show, Eq, Ord)

allDirections :: [Neighbor]
allDirections = [TopLeftN,LeftN,BottomLeftN,BottomRightN,RightN,TopRightN]

emptyBS :: BoardState
emptyBS = (Map.empty, buildG (0,0) [])
testGS0 :: GameState
testGS0 = GameState Player1 0 emptyBS startingHand startingHand
testGS1 :: GameState
testGS1 = unsafePlacePiece testGS0 (0,0,0) Spider
testGS2 :: GameState
testGS2 = unsafePlacePiece testGS1 (-1,0,0) Spider
testGS3 :: GameState
testGS3 = unsafePlacePiece testGS2 (1,0,0) Queen
testGS4 :: GameState
testGS4 = unsafePlacePiece testGS3 (-2,0,0) Queen
testGS5 :: GameState
testGS5 = unsafePlacePiece testGS4 (1,-1,0) Ant

validPlacementSpots :: GameState -> [PieceCoordinate]
validPlacementSpots gs = filter valid possibles
  where bs = gsBoard gs
        pcs = fst bs
        cs = Map.keys pcs
        possibles = nub $ concatMap (\p -> oneAway cs p Queen) cs \\ cs
        firstTurn = if gsCurrPlayer gs == Player1
                    then gsTurn gs == 0
                    else gsTurn gs == 1
        notCurrentPlayer p2 = hPlayer p2 /= gsCurrPlayer gs
        wrongPlayer c = (> 0) $ length
                        $ Map.filter notCurrentPlayer (topOfStackAdjacents c)
        topOfStackAdjacents c = Map.filterWithKey (\k _ -> topOfTheStack bs k) (adjacents c)
        adjacents c = Map.filterWithKey (\k _ -> adjacentCoords c k) pcs
        valid c
          | c `Map.member` pcs = False
          | not firstTurn && wrongPlayer c = False
          | otherwise = True

validPlacementTypes :: GameState -> [PieceType]
validPlacementTypes gs
  | not (playedBee gs) && gsCurrPlayer gs == Player1 && gsTurn gs == 6 = [Queen]
  | not (playedBee gs) && gsCurrPlayer gs == Player2 && gsTurn gs == 7 = [Queen]
  | otherwise = nub $ currPlayersHand gs

validPieceMoves :: GameState -> PieceCoordinate -> [PieceCoordinate]
validPieceMoves gs c
  | not $ playedBee gs = []
  | not $ topOfTheStack bs c = []
  | not $ oneHiveRuleSatisfied bs c = []
  | otherwise = pieceTypeMoves (Map.keys pcs) c (hPieceType $ pcs Map.! c)
  where bs = gsBoard gs
        pcs = fst bs

pieceTypeMoves :: [PieceCoordinate] -> PieceCoordinate -> PieceType -> [PieceCoordinate]
pieceTypeMoves pcs c Queen = oneSpaceMove pcs Queen c
pieceTypeMoves pcs c Ant = multiSpaceMove (const False) pcs c
pieceTypeMoves pcs c Beetle = oneSpaceMove pcs Beetle c
pieceTypeMoves pcs c Grasshopper = map (pieceHop pcs c) possibleStarts
  where possibleStarts =  filter (\d -> stackHeight pcs (getNeighbor c d) >= 1) allDirections
pieceTypeMoves pcs c Spider = multiSpaceMove (== 3) pcs c

oneSpaceMove :: [PieceCoordinate] -> PieceType -> PieceCoordinate -> [PieceCoordinate]
oneSpaceMove pcs pT c = filter (canSlide pcs c)
                      $ filter (\p -> any (adjacentCoords p) (delete c pcs))
                      $ oneAway pcs c pT \\ pcs

pieceHop :: [PieceCoordinate] -> PieceCoordinate -> Neighbor -> PieceCoordinate
pieceHop pcs c n
  | stackHeight pcs c == 0 = c
  | otherwise = pieceHop pcs (getNeighbor c n) n

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
canSlide pcs c1 c2
  | dir TopLeftN = slideable [LeftN,TopRightN]
  | dir BottomRightN = slideable [RightN,BottomLeftN]
  | dir LeftN = slideable [TopLeftN,BottomLeftN]
  | dir RightN = slideable [BottomRightN,TopRightN]
  | dir TopRightN = slideable [RightN,TopLeftN]
  | dir BottomLeftN = slideable [LeftN,BottomRightN]
  | otherwise = False
  where dir d = axialEq (getNeighbor c1 d) c2
        slideable xs = minimum (map (stackHeight pcs . getNeighbor c1) xs) <= maxStackHeight
        maxStackHeight = maximum [stackHeight pcs c2, stackHeight pcs c1 - 1]

axialEq :: PieceCoordinate -> PieceCoordinate -> Bool
axialEq (x,y,_) (x2,y2,_) = x == x2 && y==y2

stackHeight :: [PieceCoordinate] -> PieceCoordinate -> Int
stackHeight pcs c = genericLength $ filter (axialEq c) pcs

oneAway :: [PieceCoordinate] -> PieceCoordinate -> PieceType-> [PieceCoordinate]
oneAway pcs (x,y,_) Beetle = groundLevel ++ beetleLevel
  where groundLevel = oneAway pcs (x,y,0) Queen
        beetleLevel = map (\((x1,y1,_),h) -> (x1,y1,h))
                      $ filter (\(_,h) -> h >= 1)
                      $ map (\c -> (c,stackHeight pcs c)) groundLevel
oneAway _ c _ = map (getNeighbor c) allDirections

getNeighbor :: PieceCoordinate -> Neighbor -> PieceCoordinate
getNeighbor (x,y,h) TopLeftN = (x,y-1,h)
getNeighbor (x,y,h) LeftN = (x-1,y,h)
getNeighbor (x,y,h) BottomLeftN = (x-1,y+1,h)
getNeighbor (x,y,h) BottomRightN = (x,y+1,h)
getNeighbor (x,y,h) RightN = (x+1,y,h)
getNeighbor (x,y,h) TopRightN = (x+1,y-1,h)

oneHiveRuleSatisfied :: BoardState -> PieceCoordinate -> Bool
oneHiveRuleSatisfied bs = (<= 2) . length . scc . snd . removePiece bs

topOfTheStack :: BoardState -> PieceCoordinate -> Bool
topOfTheStack (pcs,_) (x,y,h) = (== h) $ maximum
                                  $ map (\(_,_,h1) -> h1)
                                  $ filter (\(x1,y1,_) -> x==x1 && y==y1)
                                  $ Map.keys pcs

adjacentCoords :: PieceCoordinate -> PieceCoordinate -> Bool
adjacentCoords c1 c2
  | axialEq c1 c2 = True
  | otherwise = any (\d -> axialEq (getNeighbor c1 d) c2) allDirections

removePiece :: BoardState -> PieceCoordinate -> BoardState
removePiece (pcs,ba) c = (newMap, newGraph)
  where
    newMap = Map.delete c pcs
    removedId = hPieceId $ pcs Map.! c
    newGraph = buildG (0,Map.size pcs-1) $ filter (\(e1,e2) -> e1 /= removedId && e2 /= removedId) (edges ba)

unsafeSkipTurn :: GameState -> GameState
unsafeSkipTurn gs = GameState (nextPlayer gs) (gsTurn gs + 1) (gsBoard gs) (gsHand1 gs) (gsHand2 gs)

unsafeMovePiece :: GameState -> PieceCoordinate -> PieceCoordinate -> GameState
unsafeMovePiece gs c1 c2 = GameState
                         { gsCurrPlayer = nextPlayer gs
                         , gsTurn = gsTurn gs + 1
                         , gsBoard = (newMap, newGraph)
                         , gsHand1 = gsHand1 gs
                         , gsHand2 = gsHand2 gs
                         }
  where
        pcs = fst bs
        ba = snd bs
        bs = gsBoard gs
        piece = pcs Map.! c1
        pieceId = hPieceId piece
        newMap = Map.insert c2 piece $ Map.delete c1 pcs
        newEdges = concatMap ((\i -> [(i,pieceId),(pieceId,i)]) . hPieceId) (Map.elems adjacents)
        oldEdges = filter (\(e1,e2) -> e1 /= pieceId && e2 /= pieceId) (edges ba)
        adjacents = Map.filterWithKey (\k _ -> adjacentCoords c2 k) pcs
        newGraph = buildG (0,Map.size newMap - 1) $ oldEdges ++ newEdges

unsafePlacePiece :: GameState -> PieceCoordinate -> PieceType -> GameState
unsafePlacePiece gs c t = GameState
                        { gsCurrPlayer = nextPlayer gs
                        , gsTurn = gsTurn gs + 1
                        , gsBoard = (newMap, newGraph)
                        , gsHand1 = hand1
                        , gsHand2 = hand2
                        }
  where
    bs = gsBoard gs
    pcs = fst bs
    ba = snd bs
    bothWays i = [(i,newPieceId),(newPieceId,i)]
    newEdges = edges ba ++ concatMap (bothWays . hPieceId) (Map.elems adjacents)
    newGraph = buildG (0,Map.size newMap -1) newEdges
    connonicalName = playerString (gsCurrPlayer gs)
                    ++ pieceString t
                    ++ if t== Queen then "" else show (numOfPieceType + 1)
    numOfPieceType = Map.size $ Map.filter (\p-> hPieceType p == t && hPlayer p == gsCurrPlayer gs) pcs
    newMap = Map.insert c (HivePiece (gsCurrPlayer gs) newPieceId t connonicalName) pcs
    newPieceId = Map.size pcs
    adjacents = Map.filterWithKey (\k _ -> adjacentCoords c k) pcs
    nextHand = delete t $ currPlayersHand gs
    hand1 = if gsCurrPlayer gs == Player1 then nextHand else gsHand1 gs
    hand2 = if gsCurrPlayer gs == Player2 then nextHand else gsHand2 gs
