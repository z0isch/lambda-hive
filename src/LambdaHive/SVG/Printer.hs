{-# LANGUAGE NoMonomorphismRestriction #-}
module LambdaHive.SVG.Printer where

-- import           Control.Concurrent
-- import           Control.Monad
-- import           Diagrams.Backend.Canvas
import qualified Data.Map             as Map
import           Diagrams.Backend.SVG
import           Diagrams.Prelude
import           LambdaHive.Types

--import qualified Graphics.Blank            as B

-- testCanvas :: Int -> B.Canvas ()
-- testCanvas w = renderDia Canvas (CanvasOptions (dims (V2 (fromIntegral $ w*50) (fromIntegral $ w*50))))
--         $ foldl (\a b -> a ||| (text "hey" # fontSizeL 0.75 <> b # rotateBy (1/12))) mempty $ replicate w (regPoly 6 1 <> circle 1.001 # opacity 0)

hex :: String -> Diagram B
hex i = text i # fontSizeL 0.75 <> regPoly 6 1 # rotateBy (1/12)
--
coordToPoint :: PieceCoordinate -> P2 Double
coordToPoint (q,r,_) =  p2 (x,y)
  where
    q' = fromIntegral q
    r' = fromIntegral r
    x = sqrt 3 * (r' + q'/2)
    y = 3/2 * q'

gameStateDiagram :: GameState -> Diagram B
gameStateDiagram gs = position m
  where m = map (\(pc,i) -> (coordToPoint pc, hex i)) $ Map.toList $ (\(pcs,_) -> Map.map hCannonicalId pcs) (gsBoard gs)

testSVG :: IO ()
testSVG = renderSVG "test.svg" (dims $ V2 300 300) (gameStateDiagram testCircle6)

mosquito :: IO (Either String (Diagram B))
mosquito = fmap image <$> loadImageEmb "mosquito.png"

-- main :: IO ()
-- main = do
--   canvas <- newMVar mempty
--   _ <- forkIO $ B.blankCanvas 3000 $ loop canvas
--   forever $ do
--     d <- readLn
--     swapMVar canvas $ testCanvas d
--
-- loop :: MVar (B.Canvas ()) -> B.DeviceContext -> IO ()
-- loop canvas context = do
--   c <- readMVar canvas
--   B.send context $ do
--     B.clearRect (0,0,B.width context,B.width context)
--     c
--   loop canvas context
