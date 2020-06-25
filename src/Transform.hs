module Transform where
import Header
import Paragraph
import Util
import List
import Code
import Meta

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

transformArticle :: String -> String -> (String, ArticleInfo)
transformArticle template source = (finalContent, (title, info, date, tags))
  where
    (title, info, date, tags) = extractMetadata source
    sourceHtml = transform source
    replacements = [("{{{ARTICLE_CONTENT}}}", sourceHtml), ("{{{ARTICLE_TITLE}}}", title), ("{{{ARTICLE_TIME}}}", showTime date), ("{{{ARTICLE_TAGS}}}", mergeTags tags)]
    finalContent = multiReplaceInString template replacements 
