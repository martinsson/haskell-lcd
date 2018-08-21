module Main where

import Lcd

main :: IO ()
main = do { 
            number <- getLine 
          ; putStrLn (toLcdString number)
          }
