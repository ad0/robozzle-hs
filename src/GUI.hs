module GUI where

import qualified Graphics.UI.Threepenny as UI
import qualified Graphics.UI.Threepenny.Canvas as Canvas
import Graphics.UI.Threepenny.Core

tileWidth :: Int
tileWidth = 50

tileHeight :: Int
tileHeight = 50

setFillColor :: Canvas.Canvas -> String -> UI ()
setFillColor canvas col = do
  _ <- element canvas # set UI.fillStyle (UI.htmlColor col)
  return ()

