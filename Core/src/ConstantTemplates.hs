module ConstantTemplates
where
import Util (if')
import Templates (rewriteTemplates, defaultTemplateStart, defaultExtractTemplateString)

data Void
type ConstantRewriter = String -> Maybe String
constantRewrite :: String -> String -> String -> Maybe String
constantRewrite word to str = if' (str == word) (Just to) Nothing

constantTemplateRewriter :: [ConstantRewriter] -> String -> Maybe (String, [Void])
constantTemplateRewriter [] _ = Nothing
constantTemplateRewriter (rule:rules) templateString
  | Just rewritten <- rule templateString = Just (rewritten, [])
  | otherwise = constantTemplateRewriter rules templateString

constantStaticTemplates :: [ConstantRewriter] ->  String -> String
constantStaticTemplates staticRules source = rewritten
  where (rewritten, []) = rewriteTemplates defaultTemplateStart defaultExtractTemplateString (constantTemplateRewriter staticRules) source
