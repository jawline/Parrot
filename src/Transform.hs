module Transform where
import Header
import Paragraph
import Util
import List

transform :: String -> String
transform [] = []
transform xs =
  case (trimLeft xs) of
    ('#':xs) -> header ++ "\n" ++ (transform rest)
      where (header, rest) = (transformHeader ('#':xs))
    ('!':'=':'!':'=':'!':xs) -> (transform (skipLine xs)) 
    ('*':xs) -> list ++ "\n" ++ (transform rest)
      where (list, rest) = (transformList ('*':xs))
    xs -> paragraph ++ "\n" ++ (transform rest)
      where (paragraph, rest) = (transformParagraph xs)
