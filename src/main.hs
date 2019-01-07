import System.IO.Error (tryIOError)
import Header
import Paragraph

input :: IO String
input = do
  c <- tryIOError getChar
  case c of
    Right(c) -> do
      remain <- input
      return (c:remain)
    Left(_) -> do
      return []

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
