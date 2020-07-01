module ConstantTemplates
where
import Util (if')
import Templates (rewriteTemplates, defaultTemplateStart, defaultExtractTemplateString)

data Void

constantRewrite :: String -> String -> String -> String
constantRewrite word to str = if' (str == word) to str

constantTemplateRewriter :: [(String -> String)] -> String -> (String, [Void])
constantTemplateRewriter staticRules templateString = (resultStr, [])
  where
    resultStr = foldr (\rule templateStr -> (rule templateStr)) templateString staticRules


constantStaticTemplates :: [(String -> String)] ->  String -> String
constantStaticTemplates staticRules source = rewritten
  where (rewritten, []) = rewriteTemplates defaultTemplateStart defaultExtractTemplateString (constantTemplateRewriter staticRules) source
