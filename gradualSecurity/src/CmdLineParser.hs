module CmdLineParser where

import System.Console.CmdArgs.GetOpt
import System.Exit
import System.IO
import Data.List (nub)

type File = String
data Flag = Static | Dynamic | Help deriving (Eq, Ord, Show, Enum, Bounded)

flags :: [OptDescr Flag]
flags =
    [ Option ['s'] []    (NoArg Static)
        "Runs the static typechecker over a file"
    , Option ['i'] []    (NoArg Dynamic)
        "Launches the interactive environment for dynamic checking"
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
