module Compiler.Lexer where

import LGPT.Helpers
import Text.Megaparsec
import Data.Bifunctor (Bifunctor(bimap, first))
import Text.Megaparsec.Char (hspace, string, printChar, alphaNumChar, digitChar,hspace1, letterChar, space1)

--These 2 instances are needed for errorBundlePretty when parsing the token stream
instance VisualStream [Compiler.Lexer.Token] where
  showTokens _ = show

instance TraversableStream [Compiler.Lexer.Token] where 
  reachOffset n posState = 
    --This compiler cannot output source file positions
    --Could be added by tagging tokens with their position during lexing?
    (
        Nothing,
        posState {
            --Move along the input
            pstateInput = take n (pstateInput posState),
            --Add the offset
            pstateOffset = pstateOffset posState + n
        }
    )


data Token =
    Keyword Keyword
    | Symbol Symbol
    | Type Type
    | Var String
    | NumLit Int
    | Space
    deriving (Show,Eq,Ord)

data Keyword =
    If
    | Printl
    | While
    deriving (Show,Eq,Ord)
data Symbol =
    Semicolon
    | OpenParen
    | CloseParen
    | OpenCurly
    | CloseCurly
    | SingleEquals
    | BinOp BinOp
    deriving (Show,Eq,Ord)
data Type =
    Int
    | Bool
    deriving (Show,Eq,Ord)
data BinOp =
    Plus
    | DoubleEquals
    | NotEqual
    deriving (Show,Eq,Ord)

--Turn the source code string into a list of token
lexicalAnalysis :: String -> Either String [Compiler.Lexer.Token]
lexicalAnalysis str =
    --Change the error bundle to a string
    --first from bifunctor makes it nice to do this
    first errorBundlePretty $ parse parseSourceCode "" str


parseSourceCode :: Parser [Compiler.Lexer.Token]
parseSourceCode =
    --The order does matter - don't want vars of keywords
    --Parsing Space as a token and removing it after makes the code cleaner
    filter (/= Space) <$> many (
        try parseSpace
        <|> try (Keyword <$> parseKeyword)
        <|> try (Type <$> parseType)
        <|> try (Symbol <$> parseSymbol)
        <|> try (Var <$> parseVar)
        <|> try (NumLit <$> parseNumLit)
        )

parseKeyword :: Parser Compiler.Lexer.Keyword
parseKeyword =
        try (parseString "if" If)
        <|> try (parseString "printl" Printl)
        <|> try (parseString "while" While)



parseSymbol :: Parser Compiler.Lexer.Symbol
parseSymbol =
        try (parseString ";" Semicolon)
        <|> try (parseString "(" OpenParen)
        <|> try (parseString ")" CloseParen)
        <|> try (parseString "{" OpenCurly)
        <|> try (parseString "}" CloseCurly)
        --Parsing == (a binary operator) before = is important
        <|> try (BinOp <$> parseBinOp)
        <|> try (parseString "=" SingleEquals)

parseBinOp :: Parser Compiler.Lexer.BinOp
parseBinOp =
    try (parseString "==" DoubleEquals)
    <|> try (parseString "!=" NotEqual)
    <|> parseString "+" Plus


parseSpace :: Parser Compiler.Lexer.Token
parseSpace =
    space1 >> pure Space

--If the string matches, return the token
parseString :: String -> a -> Parser a
parseString str tok = do
    string str >> pure tok


parseType :: Parser Compiler.Lexer.Type
parseType = do
    try (parseString "int" Compiler.Lexer.Int)
    <|> parseString "bool" Compiler.Lexer.Bool

parseVar :: Parser String
parseVar = do
    --Variables can only be letters (for simplicity)
    some letterChar

parseNumLit :: Parser Int
parseNumLit = do
    digits <- some digitChar
    pure $ read digits

