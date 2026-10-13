module Compiler.Parser where

import qualified LGPT.Helpers as Helpers
import Text.Megaparsec
import qualified Compiler.Lexer as Lexer
import Data.Void (Void)
import Data.Bifunctor (Bifunctor(first))
import Control.Monad.State.Lazy (StateT (runStateT), execStateT, MonadTrans (lift), modify, MonadState (get))
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set

{-
CFG in BNF for little language:
Prog    ::=  <Block>
Block   ::= <Stmt> <Block> 
        | epsilon
Stmt    ::= <Decl>
        | if(<Expr>){<Block>}
        | <Asg>
        | <Out>
        | while(<Expr>){<Block>}
Decl    ::= <Type> var;
Asg     ::= var SingleEquals <Expr>;
Out     ::= printl <Expr>;
Expr    ::= var
        | numlit
        | <Expr> <BinOp> <Expr>
BinOp   ::= Plus
        | DoubleEquals
        | NotEqual
Type    ::= Int
        | Bool
-}
--I think the language is unambigious 
--And I also think that it would be able to simulate Turing machines given arrays
--And hence would be Turing complete

type Var = String
newtype Prog =
    Prog Block
    deriving Show
newtype Block =
    Block [Stmt]
    deriving Show
data Stmt =
    DeclStmt Decl
    | IfStmt Expr Block
    | AsgStmt Asg
    | OutStmt Out
    | WhileStmt Expr Block
    deriving Show
data Decl =
    Decl Lexer.Type Var
    deriving Show
data Asg =
    Asg Var Expr
    deriving Show
newtype Out =
    Out Expr
    deriving Show
data Expr =
    VarExpr Var
    | BinOpExpr Lexer.BinOp Expr Expr
    | NumLitExpr Int
    deriving Show

--Parsing over the tokens from the lexer instead of strings
type Parser = Parsec Void [Lexer.Token]
--Monad transformer to add stuff to the symbol table
type StatefulParser a = StateT SymbolTable Parser a
--Stores the types of variables
type SymbolTable = Map String Lexer.Type

--From a stream of tokens, produce a parse tree and a symbol table
syntaxAnalysis :: [Lexer.Token] -> Either String (Prog,SymbolTable)
syntaxAnalysis tokenStream =
    --Change the error bundle to a string
    first errorBundlePretty $ parse (runStateT parseProg Map.empty) "" tokenStream

parseProg :: StatefulParser Prog
parseProg = do 
--Prog    ::=  <Block>
    progBlock <- parseBlock
    finished <- atEnd
    if finished then 
        pure $ Prog progBlock 
    else 
        --If we've still got tokens, then we couldn't parse something
        failure Nothing Set.empty

parseBlock ::  StatefulParser Block
parseBlock = do
--Block   ::= <Stmt> <Block> 
--        | epsilon    
    stmts <- many parseStmt
    pure $ Block stmts

parseStmt :: StatefulParser Stmt
parseStmt =
-- Stmt    ::= <Decl>
--         | if(<Expr>){<Block>}
--         | <Asg>
--         | <Out>
--         | while(<Expr>){<Block>}
    try (DeclStmt <$> parseDecl)
    <|> try parseIfStmt
    <|> try (AsgStmt <$> parseAsg)
    <|> try (OutStmt <$> parseOut)
    <|> parseWhile


parseDecl :: StatefulParser Decl
parseDecl = do
-- Decl    ::= <Type> var;
    --This language doesn't have any concept of scope
    --The variable is added to the symbol table for the whole program
    t <- parseType
    name <- variable
    single (Lexer.Symbol Lexer.Semicolon)
    --Add the variable and its type to the symbol table
    modify (Map.insert name t)
    pure $ Decl t name

parseType :: StatefulParser Lexer.Type 
-- Type    ::= Int
--         | Bool
parseType = do 
    Lexer.Type t <- 
        single (Lexer.Type Lexer.Int)
        -- <|> single (Lexer.Type Lexer.Bool)
    pure t

variable :: StatefulParser String
variable = do
    --Empty set for no errors
    token namedVariable Set.empty
    where
        namedVariable :: Lexer.Token -> Maybe String
        namedVariable =
            \case
                Lexer.Var str -> Just str
                _ -> Nothing

numLit :: StatefulParser Int
numLit = do
    --Empty set for no errors
    token numLitVal Set.empty
    where
        numLitVal :: Lexer.Token -> Maybe Int
        numLitVal =
            \case
                Lexer.NumLit x -> Just x
                _ -> Nothing

parseIfStmt :: StatefulParser Stmt
parseIfStmt = do
--if(<Expr>){<Block>}
    single (Lexer.Keyword Lexer.If)
    single (Lexer.Symbol Lexer.OpenParen)
    (expr,exprType) <- parseExpr
    if exprType /= Lexer.Bool then 
        --Expression must be a boolean
        failure Nothing Set.empty
    else do
        single (Lexer.Symbol Lexer.CloseParen)
        single (Lexer.Symbol Lexer.OpenCurly)
        block <- parseBlock
        single (Lexer.Symbol Lexer.CloseCurly)
        pure $ IfStmt expr block

parseAsg :: StatefulParser Asg
parseAsg = do
--Asg     ::= var SingleEquals <Expr>;
    lhs <- variable
    single (Lexer.Symbol Lexer.SingleEquals)
    symbolTable <- get
    (rhs,rhsType) <- parseExpr
    single (Lexer.Symbol Lexer.Semicolon)
    case Map.lookup lhs symbolTable of
        Just lhsType ->
            if lhsType == rhsType then 
                pure $ Asg lhs rhs 
            else
                --Can only assign to the same type
                failure Nothing Set.empty
        Nothing ->  
            --Undeclared variable
            failure Nothing Set.empty

    

parseWhile :: StatefulParser Stmt
parseWhile = do
--while(<Expr>){<Block>}
    single (Lexer.Keyword Lexer.While)
    single (Lexer.Symbol Lexer.OpenParen)
    (expr,exprType) <- parseExpr
    if exprType /= Lexer.Bool then
        --Expression must be a boolean 
        failure Nothing Set.empty
    else do
        single (Lexer.Symbol Lexer.CloseParen)
        single (Lexer.Symbol Lexer.OpenCurly)
        block <- parseBlock
        single (Lexer.Symbol Lexer.CloseCurly)
        pure $ WhileStmt expr block

parseOut :: StatefulParser Out
parseOut = do
--Out     ::= printl <Expr>;
    single (Lexer.Keyword Lexer.Printl)
    (expr,exprType) <- parseExpr
    single (Lexer.Symbol Lexer.Semicolon)
    pure $ Out expr

--All operators are left associative currently (+,==,!=)
--This is typical for ==
parseExpr :: StatefulParser (Expr,Lexer.Type)
parseExpr = do
--Expr     ::= var
--         | numlit
--         | <Expr> <BinOp> <Expr>
--The parsing here is very similar to the expression parsing for Ex. 6
    try parseTernaryExpr <|> try parseBinaryExpr <|> parseUnaryExpr
    where
        parseTernaryExpr :: StatefulParser (Expr,Lexer.Type)
        parseTernaryExpr = do
            (expr,te) <- parseBinaryExpr
            op   <- parseBinOp
            (x,tx)    <- parseUnaryExpr
            typeCheck op te tx (BinOpExpr op expr x)

        parseBinaryExpr :: StatefulParser (Expr,Lexer.Type)
        parseBinaryExpr = do
            (x,tx) <- parseUnaryExpr
            op <- parseBinOp
            (y,ty) <- parseUnaryExpr
            typeCheck op tx ty (BinOpExpr op x y)
        
        --Variable or numeric literal
        parseUnaryExpr :: StatefulParser (Expr,Lexer.Type)
        parseUnaryExpr = do 
            try parseVariableExpr
            <|> ((,Lexer.Int) . NumLitExpr <$> numLit)
            where
                parseVariableExpr :: StatefulParser (Expr,Lexer.Type)
                parseVariableExpr = do 
                    symbolTable <- get  
                    varName <- variable
                    --Has the variable been declared?
                    case Map.lookup varName symbolTable of 
                        Just t -> pure (VarExpr varName,t)
                        --Undeclared variable
                        Nothing -> failure Nothing Set.empty

        --Is the operator allowed on these types?
        typeCheck :: Lexer.BinOp -> Lexer.Type -> Lexer.Type -> Expr -> StatefulParser (Expr,Lexer.Type)
        typeCheck op tx ty expr 
            | op == Lexer.Plus =
                    --Plus is only defined on integers for the time being
                    if tx /= ty || tx /= Lexer.Int then 
                        --Wrong types
                        failure Nothing Set.empty
                    else 
                        pure (expr,tx)
            | op == Lexer.DoubleEquals || op == Lexer.NotEqual = 
                    --Can only compare equality on 2 things of the same type
                    if tx /= ty then 
                        --Wrong types
                        failure Nothing Set.empty 
                    else 
                        pure (expr,Lexer.Bool)


parseBinOp :: StatefulParser Lexer.BinOp
parseBinOp = do
--BinOp    ::= Plus
--         | DoubleEquals
--         | NotEqual
    Lexer.Symbol (Lexer.BinOp x) <- 
        try (single (Lexer.Symbol $ Lexer.BinOp Lexer.Plus)) 
        <|> single (Lexer.Symbol $ Lexer.BinOp Lexer.DoubleEquals)
        <|> single (Lexer.Symbol $ Lexer.BinOp Lexer.NotEqual)
    pure x

