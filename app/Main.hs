module Main where

import System.IO
import Control.Arrow (left)
import Ast
import Parser (parseLambda)
import Reduce (eval)
import TypeCheck (typeInfer, LamType)

showLeft :: Show l => Either l r -> Either String r
showLeft (Left x) = Left $ show x
showLeft (Right x) = Right x

parseEval :: String -> Either String (Value, LamType)
parseEval input = do
    expr <- showLeft $ parseLambda input
    ty <- typeInfer expr
    v <- show `left` eval expr
    return (v, ty)

repl :: IO ()
repl = do
    putStr "> "
    hFlush stdout
    input <- getLine
    case parseEval input of
        Right (v, ty) -> putStrLn $ " : " ++ show ty ++ " => " ++ show v
        Left s -> putStrLn s
    repl

fromFile :: String -> IO ()
fromFile fileName = do
    input <- readFile fileName
    case parseEval input of
        Right (v, _) -> print v
        Left s -> putStrLn s

main :: IO ()
main = repl
