module LGPT.TUI where

{-
This file is the main entry point to your coursework.

You can create or modify any files in src/ as much as you like. The 
code that is included here is a good starting point, but you don't need to 
keep it if you don't want to.
-}
import Text.Megaparsec
import Text.Megaparsec.Char
import LGPT.Helpers 
import LGPT.Numbers (parseLonghand, printLonghand)
import Data.Time (dayOfWeek)
import Data.Time.Calendar (Day,addDays, fromGregorian, diffDays, showGregorian)
import Data.Time.Clock (getCurrentTime, UTCTime (utctDay))
import Text.Read (look)
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State
import Compiler.Compiler (compile)

--------------------------------------------------------------------------------
{- | Our program. It runs a loop which:
      1. Reads a line of input
      2. Parses it into a structured Request
      3. Does something based on that request (normally printing something out).
-}
runREPL :: IO ()
runREPL =
    evalStateT statefulREPL Map.empty

statefulREPL :: StateT Memory IO ()
statefulREPL = forever $ do
  lift $ putStr prompt
  rawReq <- lift getLine
  let parsedRequest = readRequest rawReq
  respondTo parsedRequest
  
--------------------------------------------------------------------------------
-- Parsing and responding to requests:

-- | Our request type, the result of parsing a string.
data Request    = Unknown
                | Hello
                | CurrDay
                | NextDay
                | HowLongAgo Day
                | WhatIs Expr
                | Remember (MemKey,MemValue)
                | TellMe MemKey
                | Compile String String 

data Expr =   Plus Expr Expr
            | Minus Expr Expr
            | Times Expr Expr
            | Value Int
            | That

data MemKey = ThatKey     
            | StringKey String 
    deriving (Eq,Ord) --Needed to be a Data.Map key
    
data MemValue = ExprValue Int 
            | StringValue String

type Memory = Map MemKey MemValue 

type StatefulRequest = Control.Monad.State.State Memory Request

{- | Read a request. 
    This runs the parse function from Megaparsec, and
    converts any failed parses into an Unknown request.
-}
readRequest :: String -> Request
readRequest str = case parse parseRequest "" str of
            Left e -> Unknown
            Right request -> request


--Parsers for different types of requests

parseRequest :: Parser Request
parseRequest =  try parseHello
            <|> try parseCurrDay
            <|> try parseNextDay
            <|> try parseHowLongAgo
            <|> try parseWhatIs
            <|> try parseRemember
            <|> try parseTellMe
            <|> parseCompile

parseHello :: Parser Request
parseHello = do
    --"Hello(.| )"
    string "Hello"
    optional (char '.')
    eof
    pure Hello

parseCurrDay :: Parser Request
parseCurrDay = do
    --"What day is it?"
    string "What day is it?"
    eof
    pure CurrDay

parseNextDay :: Parser Request
parseNextDay = do
    --"What day is it tomorrow?"
    string "What day is it tomorrow?"
    eof
    pure NextDay

parseHowLongAgo :: Parser Request
parseHowLongAgo = do
    --"How long ago was [yyyy-mm-dd]?"
    string "How long ago was "
    date <- parseDate
    char '?'
    eof
    pure $ HowLongAgo date

parseDate :: Parser Day
parseDate = do
    --"[yyyy-mm-dd]"
    y <- replicateM 4 digitChar
    char '-'
    m <- replicateM 2 digitChar
    char '-'
    d <- replicateM 2 digitChar
    pure $ fromGregorian (read y) (read m) (read d)

parseWhatIs :: Parser Request
parseWhatIs = do
    --"What is [expr]?"
    string "What is "
    expr <- parseExpr
    char '?'
    eof
    pure $ WhatIs expr


parseExpr :: Parser Expr
parseExpr = do
    --"[expr]"
    --parseExpr parses expressions similarly to the regex
    --expr = (x([+-*]y)?)([+-*]expr)?
    try parseTernaryOp 
    <|> try parseBinaryOp 
    <|> parseValue
    where
        parseTernaryOp :: Parser Expr
        parseTernaryOp = do
            expr <- parseBinaryOp
            op   <- parseOperator
            op expr <$> parseValue
        parseBinaryOp :: Parser Expr
        parseBinaryOp = do
            x <- parseValue
            op <- parseOperator
            op x <$> parseValue

parseValue :: Parser Expr
parseValue =
    --Longhand number or "that"
    try (Value <$> parseLonghand)
    <|> (string "that" >> pure That)

parseOperator :: Parser (Expr -> Expr -> Expr)
parseOperator = do
    --[+-*]
    hspace --0 or more white space characters (excluding newlines and carriage returns)
    op <- try parseAdd <|> try parseSubtract <|> parseMultiply
    hspace
    pure op
    where
        parseAdd :: Parser (Expr -> Expr -> Expr)
        parseAdd =
            string "plus" >> pure Plus
        parseSubtract :: Parser (Expr -> Expr -> Expr)
        parseSubtract =
            string "minus" >> pure Minus
        parseMultiply :: Parser (Expr -> Expr -> Expr)
        parseMultiply =
            string "times" >> pure Times

parseRemember :: Parser Request
parseRemember = do
    --"Remember that [name] is [thing]."
    --[name] can be any sequence of printable characters that doesn't contain "is"
    --This is to prevent ambiguity between what is part of name and what is part of thing 
    --E.g. "Remember that the dog is is blue" is ambiguous
    --[thing] can be any sequence of printable characters excluding '.'s
    string "Remember that "
    key <- manyTill printChar (lookAhead (string " is"))
    string " is "
    value <- manyTill printChar (lookAhead (char '.'))
    char '.'
    eof
    pure $ Remember (StringKey key,StringValue value)

parseTellMe :: Parser Request
parseTellMe = do
    --"Tell me about [name]."
    --[thing] can be any sequence of printable characters excluding '.'s
    string "Tell me about "
    key <- manyTill printChar (lookAhead (char '.'))
    char '.'
    eof
    pure $ TellMe $ StringKey key

parseCompile :: Parser Request 
parseCompile = do 
    --"Compile [path to source code file] [path for executable file destination]"
    --E.g. "Compile ./sample_programs/fib.ll ./fib"
    string "Compile"
    hspace1  -- 1+ white space characters (excluding newlines and carriage returns)
    sourceFile <- manyTill printChar (lookAhead space1)
    hspace1 
    executableFile <- manyTill printChar (lookAhead eof)
    eof 
    pure $ Compile sourceFile executableFile



-- | Respond to a request
respondTo :: Request -> StateT Memory IO ()
respondTo Unknown = lift $ putStrLn "I don't understand that."
respondTo Hello   = lift $ putStrLn "Hi there!"

respondTo CurrDay = do
    --"Today is [day of the week]."
    currDay <- dayOfWeek <$> lift today
    lift $ putStrLn $ "Today is " ++ show currDay ++ "."

respondTo NextDay = do
    --"Tomorrow is [day of the week]."
    nextDay <- dayOfWeek . addDays 1 <$> lift today
    lift $ putStrLn $ "Tomorrow is " ++ show nextDay ++ "."

respondTo (HowLongAgo day) = do
    --"[yyyy-mm-dd] was [N] days ago."
    --Using Data.Time.Calendar.Days' definition of difference between days
    dayDiff <- (`diffDays` day) <$> lift today
    lift $ putStrLn $ showGregorian day ++ " was " ++ show dayDiff ++ " days ago."

respondTo (WhatIs expr) = do
    --"The answer is [result of evaluating expr]."
    --or "I haven't evaluated anything yet"
    --if the expression contains "that" and we haven't evaluated anything yet
    memory <- get
    --Memory used for "that"
    let exprRes = evalExpr expr memory
    case exprRes of
        --Error if expression is invalid
        Left err -> lift $ putStrLn err
        Right val -> do
            --Save the expression under ThatKey in memory 
            modify (Map.insert ThatKey (ExprValue val))
            lift $ putStrLn $ "The answer is " ++ printLonghand val ++ "."

respondTo (Remember (key,value)) = do
    --"Okay."
    --(and remembers the key with the value)
    modify (Map.insert key value)
    lift $ putStrLn "Okay."

respondTo (TellMe (StringKey key)) = do
    --"Sure - [name] is [thing]."
    --or "Sorry, I don't know anything about [name]."
    --if we haven't been told to remember [name]
    memory <- get
    lift $ putStrLn $ case Map.lookup (StringKey key) memory of
        Just (StringValue value) -> "Sure - " ++ key ++ " is " ++ value ++ "."
        _ -> "Sorry, I don't know anything about " ++ key ++ "."

respondTo (Compile src dest) = do 
    --Outputs the parse tree if everything goes well
    --or an error message otherwise
    lift $ compile src dest


--Functions used to respond to requests

today :: IO Day
today = utctDay <$> getCurrentTime

evalExpr :: Expr -> Memory -> Either String Int
evalExpr (Value x) mem   = Right x
evalExpr (Plus x y) mem  = combineExprs (+) x y mem
evalExpr (Minus x y) mem = combineExprs (-) x y mem
evalExpr (Times x y) mem = combineExprs (*) x y mem
evalExpr That mem        = 
    case Map.lookup ThatKey mem of
        Just (ExprValue val) -> Right val
        _                    -> Left "I haven't evaluated anything yet."

combineExprs :: (Int -> Int -> Int) -> Expr -> Expr -> Memory -> Either String Int
combineExprs op x y mem = do
    --Either monad instance is quite useful
    x' <- evalExpr x mem
    op x' <$> evalExpr y mem
