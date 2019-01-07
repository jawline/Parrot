module Util where
import Data.Char

trimWhiteLine :: String -> String
trimWhiteLine [] = []
trimWhiteLine('\n':xs) = ('\n':xs)
trimWhiteLine (x:xs) = if isSpace x then (trimWhiteLine xs) else (x:xs)

trimLeft :: String -> String
trimLeft [] = []
trimLeft (x:xs)
  | isSpace x = xs
  | otherwise = (x:xs)

trimRight :: String -> String
trimRight xs = reverse (trimLeft (reverse xs))

trim :: String -> String
trim xs = trimRight (trimLeft xs)
