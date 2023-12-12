module Main where

import PlutusBenchmark.BlsField.RunBlsField (runBlsField)
import System.IO (stdout)

main :: IO ()
main = runBlsField stdout