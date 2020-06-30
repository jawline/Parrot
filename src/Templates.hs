module Templates where

-- A template processor is a function which takes a template string and returns what it should appear as in the emitted text plus any additional state
type TemplateProcessor a = String -> (String, [a])

-- True on template start. Our template strings are hardcoded to the form ${{{ ... }}}
isTemplateStart ('$':'{':'{':'{':xs) = True
isTemplateStart _ = False

-- Extract the ... in ${{{ ... }}} for processing
extractTemplateString :: String -> (String -> String)
extractTemplateString ('}':'}':'}':xs) = ([], xs)
extractTemplateString (x:xs) = (x:tfollow, rest)
  where
    (tfollow, rest) = extractTemplateString xs

{-| A generic template rewriter. This tool rewrites strings in the form
${{{TEMPLATE_STR}}} by passing them to a user supplied TemplateProcessor
method for rewriting.

A template processor is supplied with the template string and should return
the desired string in the transformed text.

During rewriting additional data, such as a record of rewritten strings may
be collected (indicated by type a) and will be returned once rewriting is
finished. One example use case for this utility is collecting all of the
images referenced during templating so that images can be web-optimized
only if they are used in text. -}
rewriteTemplates :: (Eq a) => TemplateProcessor a -> String -> (String, [a])
rewriteTemplates _ [] = ([], [])
rewriteTemplates rewriter xs
  | isTemplateStart xs = (rewritten ++ follows, taggedData ++ taggedFollowing)
  | otherwise = rewriteTemplates rewriter (tail xs)
  where
    (templateStr, rest) = extractTemplateString (drop 4 xs) -- Drop 4 characters to skip ${{{
    (rewritten, taggedData) = rewriter xs
    (follows, taggedFollowing) = rewriteTemplates rewriter rest
