module Turing (Dir(Lft,Rght),Tape,Prog,run,showRun,stepRun) where 

import Data.Char
------------------------------------------------------------------------
-- (Reel [b1,b2,...,bm] c [d1,d2,...,dn]) represents the tape:
--
--     . . . B B bm ... b2 b1 c d1 d2 ... dn B B . . . 
--                            ^
data Tape = Reel String Char String

instance Show Tape where
    show (Reel left c right) = (reverse left) ++ ['[',c,']'] ++ right

-- An enumeration type for tape directions
data Dir = Rght | Lft
           deriving (Eq,Show)

-- (move dir tape) == the tape with the head moved in direction dir
move :: Dir -> Tape -> Tape
move Rght (Reel left c "")        = Reel (c:left) ' ' ""
move Rght (Reel left c (r:right)) = Reel (c:left) r right
move Lft  (Reel "" c right)       = Reel "" ' ' (c:right)
move Lft  (Reel (l:left) c right) = Reel left l (c:right)

-- (update tape c dir) == the update of tape after writing character c
--                        and then moving direction dir
update :: Tape -> Char -> Dir -> Tape
update (Reel left c right) c' d   = move d (Reel left c' right)

-- (initTape cs) == a tape with contents cs with the head on the leftmost
--                  nonblank character of the tape
initTape :: String -> Tape
initTape ""     = Reel "" ' ' ""
initTape (c:cs) = Reel "" c cs

------------------------------------------------------------------------
-- Programs

type State    = String           -- state names are Strings
type Status   = (State,Char)     -- (st,c) == the TM is in state st &
                                 --           the head is scanning c
type Action   = (Char,Dir,State) -- (c,d,st') == write char c, move dir
                                 --              d, go into state st'
type Instr    = (Status,Action)  -- a TM instruction
type Prog     = [Instr]          -- a TM program
data SnapShot = Snap State Tape  -- a snapshot of a TM at some point
type History  = [SnapShot]       -- a trace of a TM run

instance Show SnapShot where
    show (Snap state tape) = state ++ "\t " ++ (show tape) 

-- (startState prog) == the start state of the program.
--    By convention, the State of the first Instr is the start state
startState :: Prog -> State
startState (((s,_),_):_) = s

-- (run p in) == the trace of the TM with program p run on input in
run :: Prog -> String -> History
run program input 
    = step (startState program) (initTape input)
      where
        step s t@(Reel _ c _) = 
            case lookup (s,c) program of
              (Just (nc,d,ns))  ->  (Snap s t):(step ns (update t nc d))
              Nothing           ->  [Snap s t]

------------------------------------------------------------------------
-- Output

-- (showLines xs) prints each element of xs one per line
showLines :: (Show a) => [a] -> IO ()
showLines = putStr . unlines . (map show)

showRun prog input  = showLines (run prog input)

-- (stepShow xs) prints successive elements of xs when 
--   the user hits return repeatedly.
stepShow :: (Show a) => [a] -> IO ()
stepShow = helper 0
    where 
      helper _ [] = putStr ""
      helper n (x:xs) 
          = do putStr (show n ++ ": " ++ (show x)) 
               getChar
               helper (n+1) xs

stepRun prog input = stepShow (run prog input)
------------------------------------------------------------------------
