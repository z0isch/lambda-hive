module LambdaHive.Parser.Move
  ( moveParser
  , pieceParser
  , playerParser) where

import           Control.Applicative
import           LambdaHive.Types
import           Text.Trifecta

data MovePosition = MPLeft | MPRight
  deriving (Eq, Ord, Show)

neighborParser :: MovePosition -> Parser Neighbor
neighborParser MPLeft = LeftN <$ char '-'
                        <|> BottomLeftN <$ char '\\'
                        <|> TopLeftN <$ char '/'
neighborParser MPRight = RightN <$ char '-'
                        <|> BottomRightN <$ char '\\'
                        <|> TopRightN <$ char '/'

pieceTypeParser :: Parser PieceType
pieceTypeParser = Ant <$ char 'A'
                <|> Beetle <$ char 'B'
                <|> Grasshopper <$ char 'G'
                <|> Spider <$ char 'S'
                <|> Queen <$ char 'Q'

moveParser :: Parser HiveMove
moveParser = f <$> pieceParser
            <*> (char ' '
            *> (try (s <$> neighborParser MPLeft <*> pieceParser)
              <|> try (t <$> pieceParser <*> neighborParser MPRight)
              <|> try ((Nothing,Nothing) <$ char '.')
              <|> (u <$> pieceParser)))
  where
    f p1 (Just p2, Just m)= SlideMove p1 p2 m
    f p1 (Just p2, Nothing)= TopMove p1 p2
    f p1 (Nothing, _)= FirstMove p1
    u p = (Just p,Nothing)
    s c p = (Just p,Just c)
    t p c=  (Just p,Just c)

pieceParser :: Parser PieceMove
pieceParser = PieceMove <$> playerParser
            <*> (QueenMove <$ char 'Q'
                <|> (pT <$> pieceTypeParser <*> digit))
  where
    pT c n = PieceMoveType c (read [n])

playerParser :: Parser HivePlayer
playerParser = Player1 <$ char 'w' <|> Player2 <$ char 'b'
