module Main where

import Grid
import Language
import Engine
import GUI

import qualified Graphics.UI.Threepenny as UI
import qualified Graphics.UI.Threepenny.Canvas as Canvas
import Graphics.UI.Threepenny.Core
import qualified Data.Vector as V
import Data.IORef
import Control.Monad.Reader

main :: IO ()
main = startGUI defaultConfig $ setup game1 soluce1

setup :: Game -> Prog -> Window -> UI ()
setup game prog  w = do
  stateRef <- liftIO $ newIORef (game, head prog)

  _ <- return w # set title "RoboZZle-hs"
  let canvasWidth  = tileWidth  * gameSizeX game
  let canvasHeight = tileHeight * gameSizeY game
  canvas <- UI.canvas
    # set UI.width canvasWidth
    # set UI.height canvasHeight
    # set UI.style [("border", "solid 2px black")]
  buttonStep <- UI.button # set UI.text "Step"
  layout <- column [element canvas, row [element buttonStep]]
  _ <- getBody w # set children [layout]

  on UI.click buttonStep $ \_ -> do
    liftIO $ modifyIORef stateRef $ \(g,l) -> runReader (step g l) prog
    (g,_) <- liftIO $ readIORef stateRef
    redrawGame g canvas

  drawGame game canvas

drawGame :: Game -> Canvas.Canvas -> UI ()
drawGame game canvas = do
  drawGrid $ V.imap (\x c -> (x,c)) $ gameGrid game
  drawPlayer canvas game
  where drawGrid grd = V.forM_ grd drawCell'
        drawCell' (n, cell) = do
          let i = n `mod` gameSizeX game
          let j = n `div` gameSizeX game
          drawCell canvas i j cell

drawCell :: Canvas.Canvas -> Int -> Int -> Cell -> UI ()
drawCell canvas i j cell = do
  setFillColor canvas $ getHtmlColor cell
  let x = fromIntegral $ i * tileWidth
  let y = fromIntegral $ j * tileHeight
  let w = fromIntegral tileWidth
  let h = fromIntegral tileHeight
  Canvas.fillRect (x,y) w h canvas
  case cell of
    StarOn _ -> do
      setFillColor canvas "yellow"
      Canvas.beginPath canvas
      Canvas.moveTo (x + 25, y + 10) canvas
      Canvas.lineTo (x + 40, y + 25) canvas
      Canvas.lineTo (x + 25, y + 40) canvas
      Canvas.lineTo (x + 10, y + 25) canvas
      Canvas.fill canvas
    _ -> return ()

redrawGame :: Game -> Canvas.Canvas -> UI ()
redrawGame game canvas = do
  let pos@(Pos i j) = gamePos game
  drawCell canvas i j $ getCell game pos
  unless (i == 0)                  $ drawCell canvas (i-1) j $ getCell game (Pos (i-1) j)
  unless (i == gameSizeX game - 1) $ drawCell canvas (i+1) j $ getCell game (Pos (i+1) j)
  unless (j == 0)                  $ drawCell canvas i (j-1) $ getCell game (Pos i (j-1))
  unless (j == gameSizeY game - 1) $ drawCell canvas i (j+1) $ getCell game (Pos i (j+1))
  drawPlayer canvas game

drawPlayer :: Canvas.Canvas -> Game -> UI ()
drawPlayer canvas game = do
  setFillColor canvas "white"
  let x = fromIntegral $ posX (gamePos game) * tileWidth
  let y = fromIntegral $ posY (gamePos game) * tileHeight
  Canvas.beginPath canvas
  drawPlayer' x y $ gameDir game
  Canvas.fill canvas
  where drawPlayer' x y DirLeft = do
          Canvas.moveTo (x + 40, y + 10) canvas
          Canvas.lineTo (x + 40, y + 40) canvas
          Canvas.lineTo (x + 10, y + 25) canvas
        drawPlayer' x y DirRight = do
          Canvas.moveTo (x + 10, y + 10) canvas
          Canvas.lineTo (x + 40, y + 25) canvas
          Canvas.lineTo (x + 10, y + 40) canvas
        drawPlayer' x y DirUp = do
          Canvas.moveTo (x + 10, y + 40) canvas
          Canvas.lineTo (x + 25, y + 10) canvas
          Canvas.lineTo (x + 40, y + 40) canvas
        drawPlayer' x y DirDown = do
          Canvas.moveTo (x + 10, y + 10) canvas
          Canvas.lineTo (x + 40, y + 10) canvas
          Canvas.lineTo (x + 25, y + 40) canvas

