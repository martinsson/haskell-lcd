module Main where

import Lcd

main :: IO ()
main = putStrLn (toLcdString "0123456789")
