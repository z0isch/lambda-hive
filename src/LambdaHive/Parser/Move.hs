module LambdaHive.Parser.Move
  ( PieceMove(..)
  , PieceMoveId(..)
  , HiveMove(..)
  , getCanonicalId
  , moveParser
  , pieceParser
  , playerParser) where

import           Control.Applicative
import           LambdaHive.Types
import           Text.Trifecta

data PieceMove = PieceMove HivePlayer PieceMoveId
  deriving (Eq, Ord, Show)
data PieceMoveId = QueenMove | PieceMoveType PieceType Int
  deriving (Eq, Ord, Show)
data HiveMove = SlideMove PieceMove PieceMove Neighbor | TopMove PieceMove PieceMove | FirstMove PieceMove
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

leftNeighborChar :: Char -> Neighbor
leftNeighborChar '-' = LeftN
leftNeighborChar '\\' = BottomLeftN
leftNeighborChar '/' = TopLeftN
leftNeighborChar _ = error "Unexpected character"

rightNeighborChar :: Char -> Neighbor
rightNeighborChar '-' = RightN
rightNeighborChar '\\' = BottomRightN
rightNeighborChar '/' = TopRightN
rightNeighborChar _ = error "Unexpected character"

moveParser :: Parser HiveMove
moveParser = f <$> pieceParser
            <*> char ' '
            <*> (try (s <$> oneOf "-\\/" <*> pieceParser)
              <|> try (t <$> pieceParser <*> oneOf "-\\/")
              <|> try ((Nothing,Nothing) <$ char '.')
              <|> (u <$> pieceParser))
  where
    f p1 _ (Just p2, Just m)= SlideMove p1 p2 m
    f p1 _ (Just p2, Nothing)= TopMove p1 p2
    f p1 _ (Nothing, _)= FirstMove p1
    u p = (Just p,Nothing)
    s c p = (Just p,Just $ leftNeighborChar c)
    t p c=  (Just p,Just $ rightNeighborChar c)

pieceParser :: Parser PieceMove
pieceParser = PieceMove <$> playerParser <*> (try (QueenMove <$ char 'Q') <|> (pT <$> oneOf "AGBS" <*> digit))
  where
    pT c n = PieceMoveType (charPiece c) (read [n])

playerParser :: Parser HivePlayer
playerParser = charPlayer <$> oneOf "wb"
