module Engine where

import Grid
import Language

import Control.Monad.State
import Control.Monad.Reader
import Debug.Trace

type RoboM a = ReaderT Prog (State [Instr]) a

runRobo :: RoboM a -> Prog -> a
runRobo m prog = evalState (runReaderT m prog) $ head prog

step :: Game -> RoboM Game
step game = do
  traceM $ "[" ++ show (gameNbStars game) ++ "]"
  traceM $ showGame game
  Instr cond act : instrs <- get
  put instrs
  let color = case getCurrentCell game of
                Simple c -> c
                _        -> error "error"
  case cond of
    Always  -> doAction game act
    If col  -> if color == col then doAction game act else return game
  where doAction g act =
          case act of
            TurnLeft  -> return $ g { gameDir = turnLeft (gameDir g) }
            TurnRight -> return $ g { gameDir = turnRight (gameDir g) }
            GoForward -> return $ goForward g
            CallF i   -> do
                           prog   <- ask
                           instrs <- get
                           put $ (prog !! (i-1)) ++ instrs
                           return g

turnLeft :: Direction -> Direction
turnLeft DirUp    = DirLeft
turnLeft DirDown  = DirRight
turnLeft DirLeft  = DirDown
turnLeft DirRight = DirUp

turnRight :: Direction -> Direction
turnRight DirUp    = DirRight
turnRight DirDown  = DirLeft
turnRight DirLeft  = DirUp
turnRight DirRight = DirDown

goForward :: Game -> Game
goForward game = let pos = newPosition (gamePos game) (gameDir game) in
                 let (game', b) = checkCell game pos in
                 if b
                   then game' { gamePos     = pos
                              , gameNbStars = gameNbStars game - 1
                              }
                   else game' { gamePos = pos }
  where checkCell g p = case getCell g p of
                          None     -> error "fell off the puzzle"
                          StarOn _ -> (removeStar g p, True)
                          _        -> (g, False)

newPosition :: Position -> Direction -> Position
newPosition (Pos x y) DirUp    = Pos x (y-1)
newPosition (Pos x y) DirDown  = Pos x (y+1)
newPosition (Pos x y) DirLeft  = Pos (x-1) y
newPosition (Pos x y) DirRight = Pos (x+1) y
