module Templates where

-- A template processor is a function which takes a template string and returns what it should appear as in the emitted text plus any additional state
type TemplateProcessor a = String -> (String, [a])
type TemplateStart = String -> Maybe String
type TemplateExtract = String -> (String, String)

-- True on template start. Our template strings are hardcoded to the form ${{{ ... }}}
defaultTemplateStart ('$':'{':'{':'{':xs) = Just xs
defaultTemplateStart _ = Nothing

-- Extract the ... in ${{{ ... }}} for processing
defaultExtractTemplateString ('}':'}':'}':xs) = ([], xs)
defaultExtractTemplateString (x:xs) = (x:extracted, rest)
  where
    (extracted, rest) = defaultExtractTemplateString xs

{-|
A generic template rewriter. This tool rewrites strings in the form
${{{TEMPLATE_STR}}} by passing them to a user supplied TemplateProcessor
method for rewriting.

A template processor is supplied with the template string and should return
the desired string in the transformed text.

During rewriting additional data, such as a record of rewritten strings may
be collected (indicated by type a) and will be returned once rewriting is
finished. One example use case for this utility is collecting all of the
images referenced during templating so that images can be web-optimized
only if they are used in text.
-}
rewriteTemplates :: (Eq a) => TemplateStart -> TemplateExtract -> TemplateProcessor a -> String -> (String, [a])
rewriteTemplates _ _ _ [] = ([], [])
rewriteTemplates templateStart extractTemplate rewriter xs =
  case (templateStart xs) of
    Just startString -> let
      (templateStr, rest) = extractTemplate startString
      (rewritten, taggedData) = rewriter templateStr
      (follows, taggedFollowing) = rewriteMe rest
      in (rewritten ++ follows, taggedData ++ taggedFollowing)
    Nothing -> let
        (y:ys) = xs
        (follows, taggedFollows) = rewriteMe ys
      in (y:follows, taggedFollows)
  where
    rewriteMe = rewriteTemplates templateStart extractTemplate rewriter
