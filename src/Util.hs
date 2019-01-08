module Util where
import Data.Char

readToNext :: String -> Char -> Maybe (String, String)
readToNext [] _        = Nothing 
readToNext ('\n':xs) _ = Nothing
readToNext (x:xs) y
           | x == y    = Just ([], xs)
           | otherwise = case (readToNext xs y) of
              Just (part1, remaining) -> Just (x:part1, remaining)
              Nothing -> Nothing

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
