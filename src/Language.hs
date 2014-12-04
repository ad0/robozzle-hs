module Language where

import Grid

data Action = TurnLeft
            | TurnRight
            | GoForward
            | CallF Int

data Condition = Always
               | If Color

data Instr = Instr { condition :: Condition
                   , action    :: Action
                   }

type Prog = [[Instr]]

isLegalProgram :: Prog -> [Int] -> Bool
isLegalProgram fs lens
  | length lens /= length fs = False
  | otherwise                = all (uncurry (<=)) $ zip (map length fs) lens

soluce1 :: Prog
soluce1 = [f1, f2]
  where f1 = [ Instr Always GoForward
             , Instr (If Green) TurnLeft
             , Instr (If Red) TurnRight
             , Instr (If Red) (CallF 2)
             , Instr Always (CallF 1)
             ]
        f2 = [ Instr Always GoForward
             , Instr (If Green) TurnRight
             , Instr Always (CallF 2)
             ]
