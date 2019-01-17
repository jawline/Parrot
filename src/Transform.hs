module Transform where
import Header
import Paragraph
import Util
import List
import Code

transform :: String -> String
transform [] = []
transform xs =
  case (trimLeft xs) of
    ('#':xs) -> header ++ "\n" ++ (transform rest)
      where (header, rest) = (transformHeader ('#':xs))
    ('!':'=':'!':'=':'!':xs) -> (transform (skipLine xs)) 
    ('`':'`':'`':xs) -> case transformMultilineCode xs of
      Just (code, remaining) -> code ++ transform remaining
      Nothing -> '`':'`':'`':(transform xs)
    ('*':xs) -> list ++ "\n" ++ (transform rest)
      where (list, rest) = (transformList ('*':xs))
    xs -> paragraph ++ "\n" ++ (transform rest)
      where (paragraph, rest) = (transformParagraph xs)
