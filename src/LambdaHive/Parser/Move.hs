module LambdaHive.Parser.Move where

import           Control.Applicative
import           LambdaHive.Types
import           Text.Trifecta

data PieceMove = PieceMove HivePlayer PieceMoveId
  deriving (Eq, Ord, Show)
data PieceMoveId = QueenMove | PieceMoveType PieceType Int
  deriving (Eq, Ord, Show)
data HiveMove = HiveMove PieceMove PieceMove Neighbor
  deriving (Eq, Ord, Show)

getCanonicalId :: PieceMove -> String
getCanonicalId (PieceMove p QueenMove) = playerString p ++ "Q"
getCanonicalId (PieceMove p (PieceMoveType pT i)) = playerString p ++ pieceString pT ++ show i

charPiece :: Char -> PieceType
charPiece 'A' = Ant
charPiece 'Q' = Queen
charPiece 'B' = Beetle
charPiece 'G' = Grasshopper
charPiece 'S' = Spider
charPiece _ = error "Unexpected character"

charPlayer :: Char -> HivePlayer
charPlayer 'w' = Player1
charPlayer 'b' = Player2
charPlayer _ = error "Unexpected character"

moveParser :: Parser HiveMove
moveParser = f <$> pieceParser <*> char ' ' <*> (try (s <$> oneOf "-\\/" <*> pieceParser) <|> t <$> pieceParser <*> oneOf "-\\/")
  where
    f p1 _ (p2,m)= HiveMove p1 p2 m
    s c p = (p,leftNeighbor c)
    t p c= (p,rightNeighbor c)
    leftNeighbor '-' = LeftN
    leftNeighbor '\\' = BottomLeftN
    leftNeighbor '/' = TopLeftN
    leftNeighbor _ = error "Unexpected character"
    rightNeighbor '-' = RightN
    rightNeighbor '\\' = BottomRightN
    rightNeighbor '/' = TopRightN
    rightNeighbor _ = error "Unexpected character"

pieceParser :: Parser PieceMove
pieceParser = PieceMove <$> playerParser <*> (try (QueenMove <$ char 'Q') <|> (pT <$> oneOf "AGBS" <*> digit))
  where
    pT c n = PieceMoveType (charPiece c) (read [n])


playerParser :: Parser HivePlayer
playerParser = charPlayer <$> oneOf "wb"
