module Main where

import System.Console.CmdArgs.GetOpt
import System.Environment
import System.Exit
import System.IO
import Data.List (nub)


type File = String
data Flag = TypeCheck | Repl | Help deriving (Eq, Ord, Show, Enum, Bounded)

flags :: [OptDescr Flag]
flags =
    [ Option ['s'] []    (NoArg TypeCheck)
        "Runs the static typechecker over a file"
    , Option ['r'] []    (NoArg Repl)
        "Launches the interactive repl"
    , Option ['h'] []    (NoArg Help)
        "Shows this help information"
    ]

parse :: [String] -> IO ([Flag], [File])
parse argv = case getOpt Permute flags argv of
               (args, fs, []) -> do
                   let files = if null fs then ["-"] else fs
                   if Help `elem` args
                        then do hPutStrLn stderr (usageInfo header flags)
                                exitSuccess
                        else return (nub args, files)
               (_,_,errs) -> do
                    hPutStrLn stderr (concat errs ++ usageInfo header flags)
                    exitWith (ExitFailure 1)
               where header = "Usage: gradSec [-srh] [file ...]"

main :: IO ()
main = do
    (args, files) <- getArgs >>= parse 
    print args
    print files
