module Engine where

import Grid
import Language

import Control.Monad.Reader

--type RoboM a = ReaderT Prog (State [Instr]) a

--runRobo :: RoboM a -> Prog -> a
--runRobo m prog = evalState (runReaderT m prog) $ head prog

step :: Game -> [Instr] -> Reader Prog (Game, [Instr])
step game instrs = do
  let (Instr cond act : instrs') = instrs
  let color = case getCurrentCell game of
                Simple c -> c
                _        -> error "error"
  case cond of
    Always  -> doAction game instrs' act
    If col  -> if color == col then doAction game instrs' act else return (game, instrs')
  where doAction g i act =
          case act of
            TurnLeft  -> return (g { gameDir = turnLeft (gameDir g) }, i)
            TurnRight -> return (g { gameDir = turnRight (gameDir g) }, i)
            GoForward -> return (goForward g, i)
            CallF n   -> do
                           prog   <- ask
                           return (g, (prog !! (n-1)) ++ i)

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
