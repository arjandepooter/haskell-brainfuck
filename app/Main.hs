module Main where

import           Brainfuck                                ( runProgram )
import           Brainfuck.Parser                         ( parseBrainfuck )

main :: IO ()
main = do
    parseResult <- parseBrainfuck <$> getContents
    case parseResult of
        Left  err -> print err
        Right bf  -> runProgram bf
