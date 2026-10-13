module Compiler.Compiler where
import System.IO
import qualified Compiler.Lexer as Lexer
import qualified Compiler.Parser as Parser
import qualified Compiler.Generator as Generator
import qualified Data.Map as Map

compile :: String -> String -> IO ()
compile sourceName executableName = do
    fhand <- openFile sourceName ReadMode
    sourceCode <- hGetContents fhand
    case
        --Bind the either value from lexing through into syntax analysis
        Lexer.lexicalAnalysis sourceCode >>=
        \tokenStream -> Parser.syntaxAnalysis tokenStream 
        of
            Left err -> putStrLn err
            Right (parseTree,symbolTable) -> do
                print parseTree
                --Doesn't throw errors as the source code has been checked
                Generator.codeGeneration parseTree symbolTable executableName
                putStrLn "Successfully compiled"