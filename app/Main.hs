module Main where

import Lib

main :: IO ()
main = select ("first_name", "age") $ from "people.csv"
