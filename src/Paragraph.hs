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
isParagraphEnd ('\n':[]) = True
isParagraphEnd n = False

combine :: (String, String) -> (String, String) -> (String, String)
combine (l1, r1) (l2, r2) = (l1 ++ l2, r2)

transformParagraphInt :: String -> (String, String)
transformParagraphInt [] = ([], [])
transformParagraphInt (x:xs)
  | isParagraphEnd (x:xs) = ([], xs)
  | isInlineCodeStart x = case (transformInlineCode xs) of
    Just d -> combine d (transformParagraphInt remaining)
    Nothing -> combine ([x], xs) (transformParagraphInt xs)
  | isStartOfLink x = case (transformLink xs) of
    Just d -> combine d (transformParagraphInt remaining)
    Nothing -> combine ([x], xs) (transformParagraphInt xs)
  | otherwise = (x:paragraph, remaining)
    where
      (paragraph, remaining) = (transformParagraphInt xs)

transformType :: String -> String -> (String, String)
transformType elemname source = ("<" ++ elemname ++ ">" ++ (trim paragraph) ++ "</" ++ elemname ++ ">", remaining)
  where (paragraph, remaining) = (transformParagraphInt source)

transformSpan = transformType "span"
transformParagraph = transformType "p" 
