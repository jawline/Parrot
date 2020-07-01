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

transformParagraphInt :: String -> (String, String)
transformParagraphInt [] = ([], [])
transformParagraphInt (x:xs)
  | isParagraphEnd (x:xs) = ([], xs)
  | isInlineCodeStart x = combine (transformInlineCode xs)
  | Just (linkContent, remaining) <- transformLink xs = combine (linkContent, remaining)
  | otherwise = combine ([x], xs)
  where
    combine (content, rest) = let (lfollow, rfollow) = transformParagraphInt rest
      in (content ++ lfollow, rfollow)

transformType :: String -> String -> (String, String)
transformType elemname source = ("<" ++ elemname ++ ">" ++ (trim paragraph) ++ "</" ++ elemname ++ ">", remaining)
  where (paragraph, remaining) = (transformParagraphInt source)

transformSpan = transformType "span"
transformParagraph = transformType "p" 
