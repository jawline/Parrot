import System.IO.Error (tryIOError)
import Data.Char

input :: IO String
input = do
  c <- tryIOError getChar
  case c of
    Right(c) -> do
      remain <- input
      return (c:remain)
    Left(_) -> do
      return []

trimWhiteLine :: String -> String
trimWhiteLine [] = []
trimWhiteLine('\n':xs) = ('\n':xs)
trimWhiteLine (x:xs) = if isSpace x then (trimWhiteLine xs) else (x:xs)

transformHeaderInt :: String -> (String, String)
transformHeaderInt [] = ([], [])
transformHeaderInt ('\n':xs) = ([], xs)
transformHeaderInt (x:xs) = ((x:rest), remaining)
  where
    (rest, remaining) = (transformHeaderInt xs)

transformHeaderCountLevel :: String -> (Int, String)
transformHeaderCountLevel [] = (0, [])
transformHeaderCountLevel ('#':xs) = (level + 1, rest)
  where
    (level, rest) = (transformHeaderCountLevel xs)
transformHeaderCountLevel xs = (0, xs)

transformHeader :: String -> (String, String)
transformHeader source = ("<h" ++ (show level) ++ ">" ++ header ++ "</h" ++ (show level) ++ ">", remaining)
  where
    (header, remaining) = (transformHeaderInt (trimWhiteLine headerStart))
    (level, headerStart) = (transformHeaderCountLevel source)

transformParagraphInt :: String -> (String, String)
transformParagraphInt xs = (xs, [])

transformParagraph :: String -> (String, String)
transformParagraph source = ("<p>" ++ paragraph ++ "</p>", remaining)
  where
    (paragraph, remaining) = (transformParagraphInt source)

transform :: String -> String
transform [] = []
transform ('#':xs) = transformed ++ (transform rest)
  where
    (transformed, rest) = (transformHeader ('#':xs))
transform (x:xs) = paragraph ++ (transform rest)
  where
    (paragraph, rest) = (transformParagraph (x:xs)) 

main = do
  source <- input
  putStrLn (transform source)
