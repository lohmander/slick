module Main where

import           Parser
import           Text.Megaparsec

import           System.Environment
import           System.Exit



main = getArgs >>= cmdParse >>= putStr . gen

cmdParse ["-h"] = usage   >> exit
cmdParse ["-v"] = version >> exit
cmdParse []     = getContents
cmdParse fs     = concat `fmap` mapM readFile fs

usage   = putStrLn "Usage: slick [-vh] [file...]"
version = putStrLn "Slick 0.1"
exit    = exitWith ExitSuccess
die     = exitWith (ExitFailure 1)

gen :: String -> String
gen c =
    case (parse parser "./test.slick" c) of
        Left err ->
            parseErrorPretty err

        Right st ->
            show st


errorDump :: (Ord t, ShowToken t, ShowErrorComponent e) => ParseError t e -> String
errorDump (ParseError pos us ps xs) =
    show pos


