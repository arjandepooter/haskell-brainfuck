module Brainfuck.Parser
    ( parseBrainfuck
    , expressionParser
    )
where

import           Brainfuck                                ( Expr(..)
                                                          , Program
                                                          )
import           Control.Applicative                      ( (<*) )
import           Control.Monad                            ( void )
import           Text.Parsec
import           Text.Parsec.String                       ( Parser )

charToExpr :: Char -> Expr
charToExpr c = case c of
    '<' -> MoveLeft
    '>' -> MoveRight
    '+' -> Inc
    '-' -> Dec
    '.' -> Put
    ',' -> Get
    _   -> Noop

commentParser :: Parser Expr
commentParser = Comment <$> noneOf "<>+-.,[]"

loopParser :: Parser Expr
loopParser = Loop <$> between (char '[') (char ']') (many expressionParser)

expressionParser :: Parser Expr
expressionParser =
    loopParser
        <|> commentParser
        <|> (charToExpr <$> oneOf "<>+-.,")
        <?> "expression, \"[\""

parseBrainfuck :: String -> Either ParseError Program
parseBrainfuck = parse (many expressionParser <* eof) ""
