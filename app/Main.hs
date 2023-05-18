
module Main where

import System.IO
import System.Console.Isocline

import Exp
import Sugar
import Eval
import Parsing
import Printing
import Program
import REPLCommand
import Text.Parsec
import Text.Parsec.String
import qualified Data.Map.Strict as Map

--procQ Quit = return ()
--procQ (Load file) = do{putStrLn file; putStrLn "Not implemented yet"; main}
--procQ (Eval ev) = case (parse exprParser "" ev) of
--                      Left err -> do{putStrLn $ show err; putStrLn "Error: cannot parse expression"; main}
--                      Right f -> do{ putStrLn $ show (sugarExp $ normalize $ desugarExp f); main}
--
--procesare :: String -> IO ()
--procesare s = case (parse replCommand "" s) of
--                Left err -> putStrLn $ show err
--                Right f -> procQ f
--main :: IO ()
--main = do{ putStrLn "HS>"; s <- getLine; procesare s;}

procQ env Quit = return ()
procQ env (Load file) =
    do{
        putStrLn file;
        val <- parseFromFile program file;
        case val of
            Right def -> do{putStrLn $ show (programEnv def); execute (programEnv def)}
            Left err -> do{putStrLn $ show err; execute env}
      }
procQ env (Eval ev) = case (parse exprParser "" ev) of
                      Left err -> do{putStrLn $ show err; putStrLn "Error: cannot parse expression"; execute env}
                      Right f -> do{ putStrLn $ show (sugarExp $ normalizeEnv env $ desugarExp f); execute env }


procesare env s = case (parse replCommand "" s) of
                Left err -> putStrLn $ show err
                Right f -> procQ env f

execute :: Environment -> IO()
execute env = do{ putStrLn "HS>"; s <- getLine; procesare env s;}

main ::IO()
main  = execute Map.empty

