module Link where
import Util

isStartOfLink :: Char -> Bool
isStartOfLink '[' = True
isStartOfLink n = False

readLinkText  xs = (readToNext xs ']')
readLinkUrl xs = (readToNext xs ')')

transformLink :: String -> Maybe (String, String)
transformLink xs = case (readLinkText xs) of
    Just (linkText, afterUrl) ->
      case afterUrl of
        ('(':afterLParen) -> case (readLinkUrl afterLParen) of
          Just (linkUrl, remaining) -> Just ("<a href=\"" ++ linkUrl ++ "\">" ++ linkText ++ "</a>", remaining)
          Nothing -> Nothing
        otherwise -> Nothing
    Nothing -> Nothing
