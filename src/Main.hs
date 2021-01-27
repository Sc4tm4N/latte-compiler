module Main where

import System.IO (hPutStrLn, stderr)
import System.Environment (getArgs)
import System.Exit (exitFailure)

import Frontend.Grammar.ParLatte (myLexer, pProgram)
import Frontend.Grammar.AbsLatte(Program)
import Frontend.Grammar.ErrM (Err(Bad, Ok))
import Frontend.Grammar.EqLatte (ErrPos)

import Frontend.StaticAnalysis.Analyser (staticallyAnalyze)
import Backend.Compiler (compile)

import Utils.Constants (wrongUsage, parsingError, staticAnalysisError, okMsg, errorMsg)
import Frontend.Data (ParserFunction)

import System.FilePath.Posix (dropExtension)
import System.Process (callCommand, callProcess)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      putStrLn wrongUsage
    filenames -> do
      mapM_ (processFile pProgram) filenames

processFile :: ParserFunction (Program ErrPos) -> FilePath -> IO ()
processFile parser filePath = readFile filePath >>= processContent parser filePath

processContent :: ParserFunction (Program ErrPos) -> FilePath -> String -> IO ()
processContent parser filePath fileContent =
  let ts = myLexer fileContent in
    case parser ts of
      Bad badMessage -> do
        hPutStrLn stderr errorMsg
        hPutStrLn stderr parsingError
        hPutStrLn stderr badMessage
        exitFailure
      Ok parsingTree -> do
        res <- staticallyAnalyze parsingTree
        case res of
          Nothing -> do
            result <- compile parsingTree
            case result of
              Left asmBody -> do
                hPutStrLn stderr okMsg
                writeFile (extensionToS filePath) (asmBody ++ "\n")
                callProcess "gcc" ["-m32", "-O0", "-ggdb", "-c", extensionToS filePath, "-o", extensionToO filePath]
                let bashCommand = "'ld -o " ++ (noExtension filePath) ++ " -m elf_i386 " ++ (extensionToO filePath) ++ " libs/runtime.o /home/students/inf/PUBLIC/MRJP/lib32/crt1.o /home/students/inf/PUBLIC/MRJP/lib32/crti.o /home/students/inf/PUBLIC/MRJP/lib32/crtn.o -L/home/students/inf/PUBLIC/MRJP/lib32 -lc'"
                callCommand $ "/bin/bash -c " ++  bashCommand
--                callProcess "gcc" ["-m32", "-ggdb", "libs/runtime.o", extensionToS filePath, "-o", noExtension filePath]
                putStrLn $ "Success, " ++ (noExtension filePath) ++ " generated.\n"
                return ()
              Right errMsg -> do
                hPutStrLn stderr $ "ERROR\n" ++ errMsg
                exitFailure
          Just badMessage -> do
            hPutStrLn stderr errorMsg
            hPutStrLn stderr staticAnalysisError
            hPutStrLn stderr badMessage
            exitFailure

extensionToS :: FilePath -> FilePath
extensionToS fileName = (dropExtension fileName) ++ ".s"

extensionToO :: FilePath -> FilePath
extensionToO fileName = (dropExtension fileName) ++ ".o"

noExtension :: FilePath -> FilePath
noExtension fileName = (dropExtension fileName)