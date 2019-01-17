module Paragraph where
import Util
import Link
import Code

isParagraphEnd :: String -> Bool
isParagraphEnd ('\n':'\n':xs) = True
isParagraphEnd ('\n':'#':xs) = True
isParagraphEnd ('\n':'*':xs) = True
isParagraphEnd ('\n':'-':xs) = True
isParagraphEnd ('\n':'`':'`':'`':xs) = True
isParagraphEnd ('\n':'!':'=':'!':'=':'!':xs) = True
isParagraphEnd ('\n':[]) = True
isParagraphEnd n = False

combine :: (String -> (String, String)) -> (String, String) -> (String, String)
combine y (l, r) = (l ++ lfollow, rfollow)
  where (lfollow, rfollow) = y r

combinepg = combine transformParagraphInt

transformParagraphInt :: String -> (String, String)
transformParagraphInt [] = ([], [])
transformParagraphInt (x:xs)
  | isParagraphEnd (x:xs) = ([], xs)
  | isInlineCodeStart x = case (transformInlineCode xs) of
    Just d -> combinepg d
    Nothing -> combinepg ([x], xs)
  | isStartOfLink x = case (transformLink xs) of
    Just d -> combinepg d
    Nothing -> combinepg ([x], xs)
  | otherwise = combinepg ([x], xs)

transformType :: String -> String -> (String, String)
transformType elemname source = ("<" ++ elemname ++ ">" ++ (trim paragraph) ++ "</" ++ elemname ++ ">", remaining)
  where (paragraph, remaining) = (transformParagraphInt source)

transformSpan = transformType "span"
transformParagraph = transformType "p" 
