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
  | isParagraphEnd rest = ([], urlEncode rest)
  | Just codeResult <- transformInlineCode rest = combine codeResult
  | Just linkResult <- transformLink rest = combine linkResult
  | otherwise = combine (urlEncode [x], xs)
  where
    rest = (x:xs)
    combine (content, rest) = let (lfollow, rfollow) = transformParagraphInt rest
      in (content ++ lfollow, rfollow)

transformType :: String -> String -> (String, String)
transformType elemname source = ("<" ++ elemname ++ ">" ++ (trim paragraph) ++ "</" ++ elemname ++ ">", remaining)
  where (paragraph, remaining) = transformParagraphInt source

transformSpan = transformType "span"
transformParagraph = transformType "p"
