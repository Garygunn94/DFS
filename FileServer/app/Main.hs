module Main where

import System.Environment
import System.IO
import FileServer

main :: IO ()
main = do
  args <- getArgs
  let port = read $ head args :: Int
  mkApp port