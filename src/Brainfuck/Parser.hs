module Brainfuck.Parser
    ( parseBrainfuck
    , expressionParser
    )
where

import           Brainfuck                                ( Expr(..) )
import           Control.Applicative                      ( (<*)
                                                          , (*>)
                                                          )
import           Control.Monad                            ( void )
import           Text.Parsec
import           Text.Parsec.String                       ( Parser )

charToExpr :: Char -> Expr
charToExpr '<' = MoveLeft
charToExpr '>' = MoveRight
charToExpr '+' = Inc
charToExpr '-' = Dec
charToExpr '.' = Put
charToExpr ',' = Get

loopParser :: Parser Expr
loopParser = Loop <$> between (char '[') (char ']') (many expressionParser)

expressionParser :: Parser Expr
expressionParser = do
    skipMany (noneOf "<>+-.,[]")
    loopParser
        <|> (charToExpr <$> oneOf "<>+-.,")
        <?> "expression, \"[\""

parseBrainfuck :: String -> Either ParseError [Expr]
parseBrainfuck = parse (many expressionParser <* eof) ""
