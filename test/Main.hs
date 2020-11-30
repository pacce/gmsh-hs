module Main where

import Test.Node
import System.Exit

main :: IO ()
main = do
    good <- and <$> sequence [Test.Node.runTests]
    if good
        then exitSuccess
        else exitFailure
