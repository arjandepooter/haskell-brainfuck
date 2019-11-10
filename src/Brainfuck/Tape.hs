module Brainfuck.Tape
    ( newTape
    , current
    , moveLeft
    , moveRight
    , increment
    , decrement
    , setCurrent
    , Tape()
    )
where

import           Data.List                                ( intersperse )

data Tape = Tape { left :: [Int], current :: Int, right :: [Int] }

instance Show Tape where
    show (Tape left current right) =
        (unwords . fmap show . reverse) left
            ++ " [ "
            ++ show current
            ++ " ] "
            ++ (unwords . fmap show) right

newTape :: Tape
newTape = Tape [] 0 []

moveLeft :: Tape -> Tape
moveLeft (Tape []       c rs) = Tape [] 0 (c : rs)
moveLeft (Tape (l : ls) c rs) = Tape ls l (c : rs)

moveRight :: Tape -> Tape
moveRight (Tape ls c []      ) = Tape (c : ls) 0 []
moveRight (Tape ls c (r : rs)) = Tape (c : ls) r rs

increment :: Tape -> Tape
increment (Tape l c r) = Tape l (c + 1) r

decrement :: Tape -> Tape
decrement (Tape l c r) = Tape l (c - 1) r

setCurrent :: Int -> Tape -> Tape
setCurrent value (Tape l _ r) = Tape l value r
