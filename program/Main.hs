module Main where

import Codec.GMSH.Parse
import System.Environment
import Text.Parsec.Text

main :: IO ()
main = do
    { args <- getArgs
    ; result <- parseFromFile mesh (head args)
    ; case result of
        Left err -> print err
        Right ps -> print ps
    }
