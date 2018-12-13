module Main where

import Api
import System.IO 

main :: IO ()
main = do 
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout LineBuffering
  startApp
