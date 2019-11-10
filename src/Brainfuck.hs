module Brainfuck
    ( Expr(..)
    )
where

data Expr = Inc | Dec | MoveLeft | MoveRight | Get | Put | Loop [Expr] deriving (Show)
