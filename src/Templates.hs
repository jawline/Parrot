module Templates where

isTemplateStart ('$':'{':'{':'{':xs) = True
isTemplateStart _ = False

processTemplate :: String -> (String, String, [ImageExpectation])
processTemplate xs = ("", xs, [])

rewriteTemplates :: String -> (String, [ImageExpectation])
rewriteTemplates [] = []
rewriteTemplates xs
  | isTemplateStart (rewritten, rest, expectations) = (rewritten ++ (rewriteTemplate rest), expectations)
  | otherwise = rewriteTemplate (tail xs)
