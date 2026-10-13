module Compiler.Generator where

import System.Process
import qualified Compiler.Lexer as Lexer
import qualified Compiler.Parser as Parser
import qualified Data.Map as Map
import Control.Monad.State
import Data.List (intercalate)
import Data.Bifunctor (first)
import Data.Maybe (fromJust)

--Second item in the pair is the label count
--It's used for generating labels in the assembly code which are guaranteed to be unique
type Generator a = State (Parser.SymbolTable,Int) a

--Turn the parse tree and symbol table into assembly code
--Then assemble this into a final executable
codeGeneration :: Parser.Prog -> Parser.SymbolTable -> String -> IO ()
codeGeneration parseTree symbolTable executableName = do
    --Compile to x86-64 assembly
    let asmSource = evalState (generateProgram parseTree) (symbolTable,0)
    --The executable name is also the name of the assembly and object files
    writeFile (executableName ++ ".asm") asmSource
    --Assemble with nasm
    callCommand $ "nasm -f elf64 -o " ++ executableName ++ ".o " ++ executableName ++ ".asm"
    --Link
    callCommand $ "ld -o " ++ executableName ++ " " ++ executableName ++ ".o"

--Turn the parse tree to assembly
generateProgram :: Parser.Prog -> Generator String
generateProgram (Parser.Prog block) = do
    (symbolTable,_) <- get
    generatedDecls <- variableDeclarations
    generatedBlock <- generateBlock block
    pure (
        programHeader
        ++ generatedDecls
        ++ globalVars
        ++ codeHeader
        ++ indent generatedBlock 1
        ++ codeExit
        )
    where
        --Define some useful constants
        programHeader =
           "global _start\n\
            \\n\
            \SYS_WRITE   equ 1\n\
            \SYS_EXIT    equ 60\n\
            \STDOUT      equ 1\n"
        codeHeader =
           "section .text\n\
            \\n\
            \_start:\n"
        codeExit = 
            "mov rax,SYS_EXIT\n\
            \mov rdi,0\n\
            \syscall\n" --Exit code 0 for now
        globalVars = --Already in the .bss segment
            --Used for printl 
            --As only ints and bools can be printed, 50 bytes is sufficient
            "output_buffer resb 50\n"


variableDeclarations :: Generator String
variableDeclarations = do
    (symbolTable,_) <- get
    --Put all variables in the uninitialised data segment
    --All variables are global and not on the stack
    pure (
        "section .bss\n" ++
        Map.foldrWithKey declareVar [] symbolTable
        )
    where
        declareVar :: String -> Lexer.Type -> String -> String
        declareVar varName t acc =
            acc ++ varName ++
                case t of
                    --ints are 8 bytes in little language 
                    Lexer.Int -> " resq 1\n"
                    --bools are 1 byte (as expected)
                    Lexer.Bool -> " resb 1\n"

generateBlock :: Parser.Block -> Generator String
generateBlock (Parser.Block stmts) =
    mapState (first concat) (mapM generateStmt stmts)


generateStmt :: Parser.Stmt -> Generator String
generateStmt (Parser.DeclStmt decl) = pure "" --We declare all the variables at the start
generateStmt (Parser.IfStmt expr block) = do
    --Sets al to 1 if true or 0 otherwise
    (exprEvaluation,_) <- generateEval expr
    generatedBlock <- generateBlock block
    (_,labelCount) <- get
    --Labels must be unique
    let skipLabel =  "label" ++ show labelCount
    incrementLabelCount 1
    pure (
        exprEvaluation ++        
        --If al is 0 (condition is false), jump over the block
        "cmp al, 0\n\ 
        \je "++skipLabel++"\n" ++
        --Else do the block 
        generatedBlock ++
        skipLabel++":\n"
        )
generateStmt (Parser.AsgStmt asg) =
    generateAsg asg
generateStmt (Parser.OutStmt out) =
    generateOut out
generateStmt (Parser.WhileStmt expr block) = do
     --Sets al to 1 if true or 0 otherwise
    (generatedEval,_) <- generateEval expr
    generatedBlock <- generateBlock block
    (_,labelCount) <- get
    let loopLabel = "label" ++ show labelCount
    let endLabel = "label" ++ show (labelCount+1)
    incrementLabelCount 2 
    pure (
        loopLabel++":\n"++
        generatedEval++
        --If al is 0 (condition is false), jump over the block
        "cmp al, 0\n\ 
        \je "++endLabel++"\n"++
        --Else do the loop
        generatedBlock ++
        "jmp "++loopLabel++"\n" ++
        endLabel++":\n"
        )   
        

incrementLabelCount :: Int -> Generator ()
incrementLabelCount by =
     modify (\(symbolTable,lCount) -> (symbolTable,lCount+by))

--Puts the result of the evaluation in rax
generateEval :: Parser.Expr -> Generator (String,Lexer.Type)
generateEval (Parser.VarExpr varName) = do
    (symbolTable,_) <- get 
    --The variable will exist, we've checked this in parsing
    let t = fromJust $ Map.lookup varName symbolTable
    let ra = sizedRegister 'a' t
    pure ("mov "++ra++", ["++varName++"]\n",t)
generateEval (Parser.NumLitExpr numLit) =
    pure ("mov rax, "++ show numLit ++"\n",Lexer.Int)
generateEval (Parser.BinOpExpr binOp l r) = do
    (e1,t) <- generateEval l
    (e2,_) <- generateEval r
    opEval <- generateBinOp binOp
    pure (
        e1 ++
        --The value of e1 is now in rax, 
        --store the current value of rbx in rcx,
        --then move e1 to rbx
        "mov rcx, rbx\n\
        \mov rbx, rax\n" ++
        e2 ++
        --The value of e2 is now in rax
        --And the value of e1 is in rbx
        opEval ++
        --Move rcx back into rbx
        "mov rbx, rcx\n"
        ,t)
    --It doesn't matter if we use rax or al and the same for b and c in this
    --As copying rax will also copy al

--Expects first operand to be in rbx and the second to be in rax,
--Stores the result in rax
generateBinOp :: Lexer.BinOp -> Generator String
--rax = rax + rbx
generateBinOp Lexer.Plus =
    pure "add rax,rbx\n"
--al == 1 if rax==rbx, al == 0 if rax!=rbx
generateBinOp Lexer.DoubleEquals = do
    (_,labelCount) <- get
    let equalLabel = "label" ++ show labelCount
    let doneLabel = "label" ++ show (labelCount+1)
    incrementLabelCount 2
    pure (
         --Result is a boolean and so should be in al not rax
        "cmp rax,rbx\n\
        \je "++equalLabel++"\n\
        \mov al, 0\n\
        \jmp "++doneLabel++"\n"++
        equalLabel++":\n"++
        "mov al, 1\n"++
        doneLabel++":\n"
        )

generateBinOp Lexer.NotEqual = do
    (_,labelCount) <- get
    let notEqualLabel = "label" ++ show labelCount
    let doneLabel = "label" ++ show (labelCount+1)
    incrementLabelCount 2
    pure (
        "cmp rax,rbx\n\
        \jne "++notEqualLabel++"\n\
        \mov al, 0\n"++
        --False if not not equal i.e. if equal
        "jmp "++doneLabel++"\n"++
        notEqualLabel++":\n"++
        --True if not equal
        "mov al, 1\n"++
        doneLabel++":\n"
        )
       
generateAsg :: Parser.Asg -> Generator String
generateAsg (Parser.Asg varName expr) = do 
    (symbolTable,_) <- get
    --Variable exists as we've checked this in parsing
    let t = fromJust $ Map.lookup varName symbolTable
    let register = sizedRegister 'a' t
    (generatedEval,_) <- generateEval expr
    pure (
        --Puts the expression in register a
        generatedEval ++
        "mov ["++varName++"], "++register++"\n"
        )

--Gives the name of the register for the size of a given data type
--E.g. rax for int, al for bool
sizedRegister :: Char -> Lexer.Type -> String 
sizedRegister registerLetter t = 
    case t of 
        --rax,rbx..
        Lexer.Int -> "r"++[registerLetter]++"x"
        --al,bl..
        Lexer.Bool -> registerLetter:"l"

--Put the expression into the output buffer, then make a print syscall
generateOut :: Parser.Out -> Generator String
generateOut (Parser.Out expr) = do
    (generatedEval,t) <- generateEval expr 
    generatedOutput <- case t of 
            Lexer.Int -> generateIntString 
            Lexer.Bool -> generateBoolString
    pure (
        generatedEval ++
        generatedOutput ++
        "mov rax,SYS_WRITE\n\
        \mov rdi,STDOUT\n\
        \mov rsi,output_buffer\n\
        \syscall\n" --Length is already in rdx
        )
    where
        generateIntString :: Generator String
        generateIntString = do 
            --The int is in rax
            --iterate through and set bytes of output_buffer
            --Put the length in rdx
            (_,labelCount) <- get 
            let printDigitLabel = "label" ++ show labelCount
            let reverseStrLabel = "label" ++ show (labelCount+1)
            incrementLabelCount 2
            --The int is in rax
            --The code divides the int by 10 repeatedly to get digits and put them in output buffer
            --It then reverses the order of the buffer so the number is the right way round
            pure (
                --rcx is the length of the number as a string (moved to rdx at the end)
                "mov rcx,0\n" ++
                printDigitLabel++":\n\
                \mov rdx,0\n\
                \mov r10,10\n\
                \div r10\n\
                \add dl,'0'\n\
                \mov [output_buffer+rcx],dl\n\
                \inc rcx\n\
                \cmp rax,0\n\
                \jne "++printDigitLabel++"\n"++
                --r11 is i and r12 is j
                --Move i towards j, swapping chars
                "mov r11,0\n\
                \mov r12,rcx\n\
                \dec r12\n"++
                reverseStrLabel++":\n\
                \mov al,[output_buffer+r11]\n\
                \mov r13,[output_buffer+r12]\n\
                \mov [output_buffer+r11],r13b\n\
                \mov [output_buffer+r12],al\n\
                \inc r11\n\
                \dec r12\n\
                \cmp r11,r12\n\
                \jl "++reverseStrLabel++"\n"++
                --New line and null terminator
                "mov al,10\n\
                \mov [output_buffer+rcx],al\n\
                \inc rcx\n\
                \mov al,0\n\
                \mov [output_buffer+rcx],al\n\
                \inc rcx\n\
                \mov rdx,rcx\n"
                --Main function does the syscall
                )

        generateBoolString :: Generator String
        generateBoolString = 
            --Bool is in al
            --Set the output_buffer to "0\n\0" or "1\n\0" depending on the value
            --Length will always be 3, put that in rdx
            pure 
                "add al,'0'\n\
                \mov [output_buffer],al\n\
                \mov al,10\n\
                \mov [output_buffer+1],al\n\
                \mov al,0\n\
                \mov [output_buffer+2],al\n\
                \mov rdx,3\n" 
            
--Indent a block of code
indent :: String -> Int -> String
indent str depth =
    intercalate "\n" (map ((concat $ replicate (tabWidth*depth) " ") ++) (lines str)) ++ "\n"
    where
        tabWidth = 4 