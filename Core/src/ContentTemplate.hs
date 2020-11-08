module ContentTemplate where
import System.FilePath
import Data.List (isPrefixOf)
import Util (if', titleToFilename)
import Templates (rewriteTemplates, defaultExtractTemplateString)

{-|
The rewriter in this file servers the dual purposes of rewriting article template links ${{{article:article title}}} and list template links ${{{list:list name}}} into website relative paths to remove the need for hard coded links to articles and enable flexibility in output directory naming.
-}

data ContentRewriterVoid

contentTemplateStart :: String -> String -> Maybe String
contentTemplateStart target xs = if' (isPrefixOf target xs) (Just (drop (length target) xs)) Nothing

listTemplateStart :: String -> Maybe String
listTemplateStart = contentTemplateStart "${{{list:"

articleTemplateStart :: String -> Maybe String
articleTemplateStart = contentTemplateStart "${{{article:"

rewriteContentTemplate :: String -> String -> Maybe (String, [ContentRewriterVoid])
rewriteContentTemplate hostedDirStart listName = Just ((hostedDirStart </> (titleToFilename listName)), [])

rewriteContentTemplates :: String -> String -> String -> (String, [ContentRewriterVoid])
rewriteContentTemplates hostedArticles hostedLists source = rewritten
  where
    (rewrittenArticles, _) = rewriteTemplates articleTemplateStart defaultExtractTemplateString (rewriteContentTemplate hostedArticles) source
    rewrittenLists = rewriteTemplates listTemplateStart defaultExtractTemplateString (rewriteContentTemplate hostedLists) rewrittenArticles
    rewritten = rewrittenLists
