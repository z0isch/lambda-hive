{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module LambdaHive.Types where

import           AI.Gametree
import           Control.Monad
import           Data.Bimap                          (Bimap)
import qualified Data.Bimap                          as Bimap
import           Data.Graph.Inductive.Graph          (delEdges, delNode, empty,
                                                      inn, insEdges, insNode,
                                                      nodes, out)
import           Data.Graph.Inductive.PatriciaTree
import qualified Data.Graph.Inductive.Query.ArtPoint as Fgl
import           Data.List
import           Data.Map.Strict                     (Map)
import qualified Data.Map.Strict                     as Map
import           Data.Maybe
import           Data.Monoid
import           Data.Set                            (Set)
import qualified Data.Set                            as Set
import           Data.Text                           (Text)
import qualified Data.Text                           as Text
import           Safe

data HivePlayer = Player1 | Player2
  deriving (Show, Eq, Ord)

data PieceType = Ant | Queen | Beetle | Grasshopper | Spider
  deriving (Show, Eq, Ord)

type PieceId = Int
--Axial coordinates with height (x,y,height) http://www.redblobgames.com/grids/hexagons/
type PieceCoordinate = (Int,Int,Int)
type CannonicalId = Text

data HivePiece = HivePiece
  { hPlayer       :: HivePlayer
  , hPieceId      :: PieceId
  , hPieceType    :: PieceType
  , hCannonicalId :: CannonicalId
  }
  deriving (Show, Eq, Ord)

type BoardAdjecency = Gr () ()

data BoardState = BoardState
  { bsCoords          :: Map PieceCoordinate HivePiece
  , bsAdjacency       :: BoardAdjecency
  , bsIdMap           :: Bimap PieceId PieceCoordinate
  , bsCannonicalIdMap :: Bimap CannonicalId PieceCoordinate
  }
  deriving (Show, Eq)

type TurnNumber = Int
type PlayerHand = [PieceType]

data GameStatus = Win HivePlayer | Draw | InProgress
  deriving (Show, Eq, Ord)

data GameState = GameState
  { gsCurrPlayer :: HivePlayer
  , gsTurn       :: TurnNumber
  , gsBoard      :: BoardState
  , gsHand1      :: PlayerHand
  , gsHand2      :: PlayerHand
  , gsStatus     :: GameStatus
  , gsMoves      :: [(PieceCoordinate,PieceCoordinate)]
  }
  deriving (Show, Eq)

instance Transitions GameState HiveMove where
  actions = validPlayerMoves
  transition m gs = fromJust $ makeMove gs m

data Neighbor = TopLeftN
              | LeftN
              | BottomLeftN
              | BottomRightN
              | RightN
              | TopRightN
    deriving (Show, Eq, Ord)

data PieceMove = PieceMove HivePlayer PieceMoveId
  deriving (Eq, Ord, Show)

data PieceMoveId = QueenMove | PieceMoveType PieceType Int
  deriving (Eq, Ord, Show)

data HiveMove = NoOp
              | SlideMove PieceMove PieceMove Neighbor
              | TopMove PieceMove PieceMove
              | FirstMove PieceMove
  deriving (Eq, Ord, Show)

getMoveString :: HiveMove -> Text
getMoveString NoOp = ""
getMoveString (FirstMove pm) = getCannonicalId pm <> " ."
getMoveString (SlideMove pm1 pm2 n) = getCannonicalId pm1 <> " " <> nStr n pm2
  where
    nStr TopLeftN p = "/" <> getCannonicalId p
    nStr LeftN p = "-" <> getCannonicalId p
    nStr BottomLeftN p = "\\" <> getCannonicalId p
    nStr TopRightN p = getCannonicalId p <> "/"
    nStr RightN p = getCannonicalId p <> "-"
    nStr BottomRightN p = getCannonicalId p <> "\\"
getMoveString (TopMove pm1 pm2) = getCannonicalId pm1 <> " " <> getCannonicalId pm2

getCannonicalId :: PieceMove -> Text
getCannonicalId (PieceMove p QueenMove) = playerText p <> "Q"
getCannonicalId (PieceMove p (PieceMoveType pT i)) = playerText p <> pieceText pT <> Text.pack (show i)

playerText:: HivePlayer -> Text
playerText Player1 = "w"
playerText Player2 = "b"

pieceText :: PieceType -> Text
pieceText Ant = "A"
pieceText Queen = "Q"
pieceText Beetle = "B"
pieceText Grasshopper = "G"
pieceText Spider = "S"

startingHand :: [PieceType]
startingHand = [Ant,Ant,Ant,Queen,Beetle,Beetle,Grasshopper,Grasshopper,Grasshopper,Spider,Spider]

getHivePieceFromCannonicalId :: GameState -> CannonicalId -> HivePiece
getHivePieceFromCannonicalId gs cId = bsCoords bs Map.! pieceCoord
  where
    bs = gsBoard gs
    pieceCoord = bsCannonicalIdMap bs Bimap.! cId

getHivePieceFromId :: GameState -> PieceId -> HivePiece
getHivePieceFromId gs pId = bsCoords bs Map.! pieceCoord
  where
    bs = gsBoard gs
    pieceCoord = bsIdMap bs Bimap.! pId

gameOver :: GameState -> Bool
gameOver gs = case status of
  InProgress -> False
  _ -> True
  where status = gameStatus (gsBoard gs)

gameStatus :: BoardState -> GameStatus
gameStatus bs
  | player1Win && player2Win = Draw
  | player1Win = Win Player1
  | player2Win = Win Player2
  | otherwise = InProgress
  where
    pcs = bsCoords bs
    queen player = headMay $ Map.toList $ Map.filter (\p -> hPieceType p == Queen && hPlayer p == player) pcs
    queenSurrounded q = case q of
      Nothing -> False
      Just p -> all (flip Map.member pcs . getNeighbor (fst p)) allDirections
    player1Win = queenSurrounded (queen Player2)
    player2Win = queenSurrounded (queen Player1)

getPieceCoord :: GameState -> CannonicalId -> Maybe PieceCoordinate
getPieceCoord gs i = Bimap.lookup i (bsCannonicalIdMap $ gsBoard gs)

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

allDirections :: [Neighbor]
allDirections = [TopLeftN,LeftN,BottomLeftN,BottomRightN,RightN,TopRightN]

initGS :: GameState
initGS = GameState Player1 0 (BoardState Map.empty empty Bimap.empty Bimap.empty) startingHand startingHand InProgress []

getPieceMoveIdFromHivePiece :: HivePiece -> PieceMoveId
getPieceMoveIdFromHivePiece hp
  | pieceType == Queen = QueenMove
  | otherwise = PieceMoveType pieceType (read [Text.last $ hCannonicalId hp])
  where pieceType = hPieceType hp

genGameState :: GameState -> [HiveMove] -> Maybe GameState
genGameState = foldM makeMove

makeMove :: GameState -> HiveMove -> Maybe GameState
makeMove gs (SlideMove piece1@(PieceMove _ pm1) piece2 dir) =
  case foundPiece1 of
    Just fp1 -> if isJust foundPiece2
      then Just $ unsafeMovePiece gs fp1 (mx,my,0)
      else Nothing
    Nothing -> if isJust foundPiece2
      then case pm1 of
        QueenMove -> Just $ unsafePlacePiece gs (mx,my,0) Queen
        PieceMoveType pt1 _ -> Just $ unsafePlacePiece gs (mx,my,0) pt1
      else Nothing
  where
    foundPiece1 = getPieceCoord gs (getCannonicalId piece1)
    foundPiece2 = getPieceCoord gs (getCannonicalId piece2)
    (mx,my,_) = getNeighbor (fromJust foundPiece2) dir
makeMove gs (TopMove piece1 piece2)
  | isJust foundPiece1 && isJust foundPiece2 = Just $ unsafeMovePiece gs (fromJust foundPiece1) (x2,y2,stackHeight' gs fp2)
  | otherwise = Nothing
  where
    foundPiece1 = getPieceCoord gs (getCannonicalId piece1)
    foundPiece2 = getPieceCoord gs (getCannonicalId piece2)
    fp2@(x2,y2,_) = fromJust foundPiece2
makeMove gs (FirstMove (PieceMove _ QueenMove)) = Just $ unsafePlacePiece gs (0,0,0) Queen
makeMove gs (FirstMove (PieceMove _ (PieceMoveType pT _))) = Just $ unsafePlacePiece gs (0,0,0) pT
makeMove gs NoOp = Just gs

validPlayerMoves :: GameState -> [HiveMove]
validPlayerMoves gs
  | gameOver gs = []
  | gsTurn gs == 0 = map genFirstMove (validPlacementTypes gs)
  | otherwise = allValidPieceMoves ++ allValidPlacements
    where
          genFirstMove pT = FirstMove (PieceMove (gsCurrPlayer gs) newPiece)
            where newState = unsafePlacePiece gs (0,0,0) pT
                  newPiece = getPieceMoveIdFromHivePiece $ bsCoords (gsBoard newState) Map.!(0,0,0)
          allSpots = concat $ permutations (validPlacementSpots gs)
          allTypes = concat $ permutations (validPlacementTypes gs)
          playersPieces = map fst $ Map.toList $ Map.filter (\p -> hPlayer p == gsCurrPlayer gs) (bsCoords $ gsBoard gs)
          allValidPieceMoves = concatMap (\pc -> map (genPieceMoves pc) (validPieceMoves gs pc)) playersPieces
          allValidPlacements = zipWith genPiecePlacement allSpots allTypes
          genPieceMoves pc1 pc2@(x2,y2,h2)
            | h2 > 0 = TopMove (PieceMove (gsCurrPlayer gs) movePiece) (PieceMove (hPlayer onBottomPiece) (getPieceMoveIdFromHivePiece onBottomPiece))
            | otherwise = SlideMove (PieceMove (gsCurrPlayer gs) movePiece) (PieceMove p sp) (oppositeNeighbor d)
            where movePiece = getPieceMoveIdFromHivePiece $ bsCoords (gsBoard gs) Map.! pc1
                  onBottomPiece = bsCoords (gsBoard gs) Map.! (x2,y2,0)
                  (d,p,sp) = neighbor gs pc2
          genPiecePlacement pC pT = slideMove newPiece (neighbor newState pC)
            where newState = unsafePlacePiece gs pC pT
                  newPiece = getPieceMoveIdFromHivePiece $ bsCoords (gsBoard newState) Map.! pC
          slideMove piece (d,p,sp) = SlideMove (PieceMove (gsCurrPlayer gs) piece) (PieceMove p sp) (oppositeNeighbor d)
          neighbor state pC = head
                  $ map (\(d,pc) ->
                          ( d
                          , hPlayer $ bsCoords (gsBoard state) Map.! pc
                          , getPieceMoveIdFromHivePiece $ bsCoords (gsBoard state) Map.! pc))
                  $ filter (\(_,pc) -> Map.member pc $ bsCoords $ gsBoard state)
                  $ map (\d -> (d,getNeighbor pC d)) allDirections


validPlacementSpots :: GameState -> [PieceCoordinate]
validPlacementSpots gs = filter valid possibles
  where bs = gsBoard gs
        pcs = bsCoords bs
        cs = Map.keys pcs
        possibles = nub $ concatMap (oneAway cs Queen) cs \\ cs
        firstTurn = if gsCurrPlayer gs == Player1
                    then gsTurn gs == 0
                    else gsTurn gs == 1
        notCurrentPlayer p2 = hPlayer p2 /= gsCurrPlayer gs
        wrongPlayer c = (> 0) $ length
                        $ Map.filter notCurrentPlayer (topOfStackAdjacents c)
        topOfStackAdjacents c = Map.filterWithKey (\k _ -> topOfTheStack bs k) (adjacents c)
        adjacents c = Map.filterWithKey (\k _ -> adjacentCoords c k) pcs
        valid c@(_,_,h)
          | c `Map.member` pcs = False
          | not firstTurn && wrongPlayer c = False
          | h > 0 = False
          | otherwise = True

validPlacementTypes :: GameState -> [PieceType]
validPlacementTypes gs
  | gsTurn gs == 0 || gsTurn gs == 1 = delete Queen $ nub $ currPlayersHand gs
  | not (playedBee gs) && gsCurrPlayer gs == Player1 && gsTurn gs == 6 = [Queen]
  | not (playedBee gs) && gsCurrPlayer gs == Player2 && gsTurn gs == 7 = [Queen]
  | otherwise = nub $ currPlayersHand gs

validPieceMoves :: GameState -> PieceCoordinate -> [PieceCoordinate]
validPieceMoves gs c
  | not $ playedBee gs = []
  | not $ topOfTheStack bs c = []
  | not $ oneHiveRuleSatisfied bs c = []
  | otherwise = pieceTypeMoves (Map.keys pcs) c (hPieceType $ pcs Map.! c )
  where bs = gsBoard gs
        pcs = bsCoords bs

pieceTypeMoves :: [PieceCoordinate] -> PieceCoordinate -> PieceType -> [PieceCoordinate]
pieceTypeMoves pcs c Queen = oneSpaceMove pcs Queen c
pieceTypeMoves pcs c Ant = multiSpaceMove (const False) pcs c
pieceTypeMoves pcs c Beetle = oneSpaceMove pcs Beetle c
pieceTypeMoves pcs c Grasshopper = map (pieceHop pcs c) possibleStarts
  where possibleStarts =  filter (\d -> stackHeight pcs (getNeighbor c d) >= 1) allDirections
pieceTypeMoves pcs c Spider = multiSpaceMove (== 3) pcs c

pieceHop :: [PieceCoordinate] -> PieceCoordinate -> Neighbor -> PieceCoordinate
pieceHop pcs c n
  | stackHeight pcs c == 0 = c
  | otherwise = pieceHop pcs (getNeighbor c n) n

oneSpaceMove :: [PieceCoordinate] -> PieceType -> PieceCoordinate -> [PieceCoordinate]
oneSpaceMove pcs pT c@(pX,pY,pH) = filter (canSlide pcs c) possibleSpaces
  where possibleSpaces = possibleGroundSpaces ++ possibleOnTopSpaces
        possibleOnTopSpaces = filter (\(_,_,h) -> h > 0) adjacentSquares
        possibleGroundSpaces = (adjacentSquares `intersect` adjacentToNeighbors) \\ pcs
        adjacentToNeighbors = concatMap (oneAway pcs Queen) neighbors
        neighbors = adjacentSquares `intersect` pcs
        adjacentSquares = oneAway pcs pT c ++ bottomOfStack
        bottomOfStack
          | pH > 0 = [(pX,pY,0)]
          | otherwise = []

multiSpaceMove :: (Int -> Bool) -> [PieceCoordinate] -> PieceCoordinate -> [PieceCoordinate]
multiSpaceMove f pcs orig = go pcs 0 Set.empty orig
  where
    go :: [PieceCoordinate] -> Int -> Set PieceCoordinate -> PieceCoordinate -> [PieceCoordinate]
    go npcs step visited c
      | f (step+1) = newSpots
      | null traversed = delete orig $ Set.toList visited
      | otherwise = nub traversed
      where
            traversed = concatMap (\s -> go (newPcs s) (step+1) newVisited s) newSpots
            newSpots = filter (`Set.notMember` visited)
                     $ oneSpaceMove npcs Queen c
            newVisited = Set.insert c visited
            newPcs s = s:delete c npcs

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

stackHeight' :: GameState -> PieceCoordinate -> Int
stackHeight' gs = stackHeight pcs
  where pcs = map fst $ Map.toList (bsCoords $ gsBoard gs)

oneAway :: [PieceCoordinate] -> PieceType -> PieceCoordinate -> [PieceCoordinate]
oneAway pcs Beetle (x,y,_)  = groundLevel ++ beetleLevel
  where groundLevel = oneAway pcs Queen (x,y,0)
        beetleLevel = map (\((x1,y1,_),h) -> (x1,y1,h))
                      $ filter (\(_,h) -> h >= 1)
                      $ map (\c -> (c,stackHeight pcs c)) groundLevel
oneAway _ _ c = map (getNeighbor c) allDirections

getNeighbor :: PieceCoordinate -> Neighbor -> PieceCoordinate
getNeighbor (x,y,h) TopLeftN = (x,y-1,h)
getNeighbor (x,y,h) LeftN = (x-1,y,h)
getNeighbor (x,y,h) BottomLeftN = (x-1,y+1,h)
getNeighbor (x,y,h) BottomRightN = (x,y+1,h)
getNeighbor (x,y,h) RightN = (x+1,y,h)
getNeighbor (x,y,h) TopRightN = (x+1,y-1,h)

oppositeNeighbor :: Neighbor -> Neighbor
oppositeNeighbor TopLeftN = BottomRightN
oppositeNeighbor LeftN = RightN
oppositeNeighbor BottomLeftN = TopRightN
oppositeNeighbor BottomRightN = TopLeftN
oppositeNeighbor RightN = LeftN
oppositeNeighbor TopRightN = BottomLeftN

topOfTheStack :: BoardState -> PieceCoordinate -> Bool
topOfTheStack (BoardState pcs _ _ _) (x,y,h) = (== h) $ maximum
                                  $ map (\(_,_,h1) -> h1)
                                  $ filter (\(x1,y1,_) -> x==x1 && y==y1)
                                  $ Map.keys pcs

adjacentCoords :: PieceCoordinate -> PieceCoordinate -> Bool
adjacentCoords c1 c2
  | axialEq c1 c2 = True
  | otherwise = any (\d -> axialEq (getNeighbor c1 d) c2) allDirections

oneHiveRuleSatisfied :: BoardState -> PieceCoordinate -> Bool
oneHiveRuleSatisfied (BoardState pcs ba _ _) c = pieceId `notElem` Fgl.ap ba
  where pieceId = hPieceId $ pcs Map.! c

removePiece :: BoardState -> PieceCoordinate -> BoardState
removePiece (BoardState pcs ba ids cIds) c = BoardState newMap newGraph newIds newCIds
  where
    newMap = Map.delete c pcs
    removedId = hPieceId coordRemoved
    coordRemoved= pcs Map.! c
    newIds = Bimap.delete removedId ids
    newCIds = Bimap.delete (hCannonicalId coordRemoved) cIds
    newGraph = delEdges deletedEdges $ delNode removedId ba
    deletedEdges = map (\(a,b,_) -> (a,b)) $ out ba removedId ++ inn ba removedId

unsafeSkipTurn :: GameState -> GameState
unsafeSkipTurn gs = GameState (nextPlayer gs) (gsTurn gs + 1) (gsBoard gs) (gsHand1 gs) (gsHand2 gs) (gsStatus gs) (gsMoves gs)

unsafeMovePiece :: GameState -> PieceCoordinate -> PieceCoordinate -> GameState
unsafeMovePiece gs c1 c2 = GameState
                         { gsCurrPlayer = nextPlayer gs
                         , gsTurn = gsTurn gs + 1
                         , gsBoard = newBoardState
                         , gsHand1 = gsHand1 gs
                         , gsHand2 = gsHand2 gs
                         , gsStatus = newStatus
                         , gsMoves = gsMoves gs ++ [(c1,c2)]
                         }
  where
        pcs = bsCoords bs
        ba = bsAdjacency bs
        bs = gsBoard gs
        piece = pcs Map.! c1
        pieceId = hPieceId piece
        cPieceId = hCannonicalId piece
        newMap = Map.insert c2 piece $ Map.delete c1 pcs
        t = containsAllVertices (delEdges deletedEdges ba) newEdges
        deletedEdges = map (\(a,b,_) -> (a,b)) $ out ba pieceId ++ inn ba pieceId
        newEdges = concatMap ((\i -> [(i,pieceId,()),(pieceId,i,())]) . hPieceId)
                 $ Map.elems adjacents
        adjacents = Map.filterWithKey (\k hp -> hPieceId hp /= pieceId && adjacentCoords c2 k) newMap
        newGraph = if t then insEdges newEdges $ delEdges deletedEdges ba else undefined
        newBoardState = BoardState newMap newGraph newIds newCIds
        newStatus = gameStatus newBoardState
        newIds = Bimap.adjust (const c2) pieceId $ bsIdMap bs
        newCIds = Bimap.adjust (const c2) cPieceId $ bsCannonicalIdMap bs

containsAllVertices :: Gr () () -> [(Int,Int,())] -> Bool
containsAllVertices gr es = all (`elem` nodes gr) ns
  where ns = nub $ map (\(x,_,_) -> x) es ++ map (\(_,y,_) -> y) es

unsafePlacePiece :: GameState -> PieceCoordinate -> PieceType -> GameState
unsafePlacePiece gs c t = GameState
                        { gsCurrPlayer = nextPlayer gs
                        , gsTurn = gsTurn gs + 1
                        , gsBoard = newBoardState
                        , gsHand1 = hand1
                        , gsHand2 = hand2
                        , gsStatus = newStatus
                        , gsMoves = gsMoves gs ++ [((0,0,-1),c)]
                        }
  where
    bs = gsBoard gs
    pcs = bsCoords bs
    ba = bsAdjacency bs
    bothWays i = [(i,newPieceId,()),(newPieceId,i,())]
    test = containsAllVertices (insNode (Map.size newMap - 1, ()) ba) newEdges
    newEdges = concatMap (bothWays . hPieceId) (Map.elems adjacents)
    newGraph = if test then insEdges newEdges $ insNode (Map.size newMap - 1, ()) ba else undefined
    connonicalName = playerText (gsCurrPlayer gs)
                    <> pieceText t
                    <> if t== Queen then "" else Text.pack (show (numOfPieceType + 1))
    numOfPieceType = Map.size $ Map.filter (\p-> hPieceType p == t && hPlayer p == gsCurrPlayer gs) pcs
    newMap = Map.insert c (HivePiece (gsCurrPlayer gs) newPieceId t connonicalName) pcs
    newPieceId = Map.size pcs
    adjacents = Map.filterWithKey (\k hp -> hPieceId hp /= newPieceId && adjacentCoords c k) newMap
    nextHand = delete t $ currPlayersHand gs
    hand1 = if gsCurrPlayer gs == Player1 then nextHand else gsHand1 gs
    hand2 = if gsCurrPlayer gs == Player2 then nextHand else gsHand2 gs
    newBoardState = BoardState newMap newGraph newIds newCIds
    newStatus = gameStatus newBoardState
    newIds = Bimap.insert newPieceId c $ bsIdMap bs
    newCIds = Bimap.insert connonicalName c $ bsCannonicalIdMap bs
