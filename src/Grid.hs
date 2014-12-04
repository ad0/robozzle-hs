module Grid where

import qualified Data.Vector as V

data Direction = DirUp | DirDown | DirLeft | DirRight

data Position = Pos { posX :: Int, posY :: Int }
                deriving (Show)

data Color = Blue | Red | Green
             deriving (Eq)

data Cell = None
          | Simple Color
          | StarOn Color
          deriving (Eq)

type Grid = V.Vector Cell

data Game = Game { gameGrid    :: Grid
                 , gameSizeX   :: Int
                 , gameSizeY   :: Int
                 , gamePos     :: Position
                 , gameDir     :: Direction
                 , gameNbStars :: Int
                 }

blue :: Cell
blue = Simple Blue

red :: Cell
red = Simple Red

green :: Cell
green = Simple Green

game1 :: Game
game1 = Game grid 16 12 (Pos 7 6) DirRight 1
  where grid = V.fromList $ replicate 48 None
               ++ [None,None,None,None,None,green,blue,blue,blue,blue,blue,blue,green,None,None,None]
               ++ [None,None,None,None,None,blue,None,None,None,None,None,None,blue,None,None,None]
               ++ [None,None,None,None,None,blue,None,None,None,None,None,None,blue,None,None,None]
               ++ [None,None,None,None,None,blue,None,blue,blue,blue,blue,blue,green,None,None,None]
               ++ [None,None,None,None,None,blue,None,None,None,None,None,None,None,None,None,None]
               ++ [None,None,None,StarOn Blue,None,green,red,green,None,None,None,None,None,None,None,None]
               ++ [None,None,None,blue,None,None,blue,None,None,None,None,None,None,None,None,None]
               ++ [None,None,None,green,blue,blue,green,None,None,None,None,None,None,None,None,None]
               ++ replicate 16 None

getCurrentCell :: Game -> Cell
getCurrentCell game = getCell game (gamePos game)

getCell :: Game -> Position -> Cell
getCell game (Pos x y) = gameGrid game V.! (gameSizeX game * y + x)

removeStar :: Game -> Position -> Game
removeStar game pos@(Pos x y) =
  let c = case getCell game pos of
            StarOn c' -> Simple c'
            _         -> error "no star to remove"
  in game { gameGrid = gameGrid game V.// [(gameSizeX game * y + x, c)] }

isSolved :: Game -> Bool
isSolved = (0 ==) . gameNbStars

showGame :: Game -> String
showGame game = showGrid 0 $ gameGrid game
  where showGrid gy grid
          | V.length grid == gameSizeX game = showLine gy grid
          | otherwise = let (v1,v2) = V.splitAt (gameSizeX game) grid
                        in showLine gy v1 ++ "\n" ++ showGrid (gy+1) v2
        showLine y line
          | y == posY (gamePos game) = let vec = V.map showCell line in
                                       V.toList $
                                         vec V.// [(posX (gamePos game), showDir (gameDir game))]
          | otherwise = V.toList $ V.map showCell line
        showCell None           = '.'
        showCell (Simple Blue)  = 'b'
        showCell (Simple Red)   = 'r'
        showCell (Simple Green) = 'g'
        showCell (StarOn Blue)  = 'B'
        showCell (StarOn Red)   = 'R'
        showCell (StarOn Green) = 'G'
        showDir DirUp    = '^'
        showDir DirDown  = 'v'
        showDir DirLeft  = '<'
        showDir DirRight = '>'

