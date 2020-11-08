module Transform where
import System.FilePath

import Header
import Paragraph
import Util
import List
import Code
import Meta
import Paths
import ConstantTemplates (constantRewrite, constantStaticTemplates)

{-|
  Transform a piece of markdown to a HTML fragment
-}
transformMarkdown :: String -> String
transformMarkdown [] = []
transformMarkdown xs =
  case (trimLeft xs) of
    ('#':xs) -> header ++ "\n" ++ (transformMarkdown rest)
      where (header, rest) = (transformHeader ('#':xs))
    ('!':'=':'!':'=':'!':xs) -> (transformMarkdown (skipLine xs))
    ('`':'`':'`':xs) -> combined (transformMultilineCode xs)
    ('*':xs) -> combined (transformList ('*':xs))
    xs -> combined (transformParagraph xs)
  where
    combined (x, r) = x ++ "\n" ++ (transformMarkdown r)

{-|
  The series of string replacements that transform the template article to a rendered article
-}
articleReplacements source info = [
  (constantRewrite "ARTICLE_CONTENT" source),
  (constantRewrite "ARTICLE_TITLE"  (articleTitle info)),
  (constantRewrite "ARTICLE_TIME" (showTime (articleDate info))),
  (constantRewrite "ARTICLE_TAGS" (mergeTags (articleTags info)))]


{-|
  Transform an article source markdown to a HTML fragment and article info
-}
transformArticle :: String -> String -> (String, ArticleInfo)
transformArticle template source = (finalContent, articleInfo)
  where
    articleInfo = extractMetadata source
    sourceHtml = transformMarkdown source
    replacements = articleReplacements sourceHtml articleInfo
    finalContent = constantStaticTemplates replacements template


{-|
  The template string replacements for list items
-}
listItemReplacements info =
  [(constantRewrite "LI_NAME" (articleTitle info)),
   (constantRewrite "LI_DESCRIPTION" (articleIntro info)),
   (constantRewrite "LI_DATE" (showTime (articleDate info))),
   (constantRewrite "LI_TAGS" (mergeTags (articleTags info))),
   (constantRewrite "LI_TARGET" (templateArticlesPath </> (titleToFilename (articleTitle info))))]

{-|
  Translates the list item template and an ArticleInfo instance into a list item HTML fragment.
-}
transformListItem :: String -> ArticleInfo -> String
transformListItem template item = constantStaticTemplates replacements template
  where
    replacements = listItemReplacements item
