module Brainfuck
    ( Expr(..)
    , Program
    , runProgram
    )
where

import           Brainfuck.Tape
import           Control.Monad                            ( foldM
                                                          , when
                                                          )
import           Data.Char                                ( ord
                                                          , chr
                                                          )
import           System.IO

data Expr = Inc | Dec | MoveLeft | MoveRight | Get | Put | Loop [Expr] deriving (Show)
type Program = [Expr]

-- quick fix to reopen stdin
getChar' :: IO Char
getChar' = openFile "/dev/tty" ReadMode >>= hGetChar

runExpr :: Expr -> Tape -> IO Tape
runExpr expr tape = case expr of
    Inc       -> return (increment tape)
    Dec       -> return (decrement tape)
    MoveLeft  -> return (moveLeft tape)
    MoveRight -> return (moveRight tape)
    Get       -> do
        val <- ord <$> getChar'
        return $ setCurrent val tape
    Put -> do
        let val = current tape
        when (val >= 0) (putChar (chr val))
        return tape
    Loop exprs -> if current tape == 0
        then return tape
        else runProgram' tape exprs >>= runExpr expr

runProgram' :: Tape -> Program -> IO Tape
runProgram' = foldM (flip runExpr)

runProgram :: Program -> IO ()
runProgram program = do
    runProgram' newTape program
    return ()
