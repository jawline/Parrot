module Util where
import Data.Char
import System.IO.Error (tryIOError)

input :: IO String
input = do
  c <- tryIOError getChar
  case c of
    Right(c) -> do
      remain <- input
      return (c:remain)
    Left(_) -> 
      return []

endOfLine :: String -> String
endOfLine [] = []
endOfLine ('\n':xs) = []
endOfLine (x:xs) = x:(endOfLine xs)

matches :: String -> String -> Bool
matches target string = take (length target) string == target

replaceInString :: String -> String -> String -> String
replaceInString [] _ _ = []
replaceInString (x:xs) target with
  | matches target (x:xs) = replaceInString (with ++ (drop (length target) (x:xs))) target with
  | otherwise = x:(replaceInString xs target with)

readToNext :: String -> (String -> Bool) -> Maybe (String, String)
readToNext [] _        = Nothing
readToNext (x:xs) y = if y(x:xs)
  then Just ([], xs)
  else case (readToNext xs y) of
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

skipLine :: String -> String
skipLine [] = []
skipLine ('\n':xs) = xs
skipLine (x:xs) = skipLine xs

fromString :: String -> String -> String
fromString target (x:xs) =
  case matches target (x:xs) of
    True -> (x:xs)
    False -> fromString target xs

untilString :: String -> String -> String
untilString target xs = reverse (fromString (reverse target) (reverse xs))

findLine :: String -> String -> String
findLine target source = endOfLine (fromString target source)

unique [] = []
unique (x:xs) = if elem x r then r else x:r
  where r = unique xs 

titleToFilename [] = ""
titleToFilename (' ':xs) = '_':(titleToFilename xs)
titleToFilename (x:xs) 
  | (isLetter x) || (isNumber x) = x:(titleToFilename xs)
  | otherwise = titleToFilename xs

indexedReverse [] = []
indexedReverse (x:[]) = [(0, x)]
indexedReverse (x:xs) = (l + 1, x):(indexedReverse xs)
  where
    ((l, _):_) = indexedReverse xs

indexed xs = reverse (indexedReverse (reverse xs))
