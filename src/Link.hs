module Link where
import Util

isStartOfLink :: Char -> Bool
isStartOfLink '[' = True
isStartOfLink n = False

readLinkUrl  xs = (readToNext xs ']')
readLinkText xs = (readToNext xs ')')

transformLink :: String -> Maybe (String, String)
transformLink xs = case (readLinkUrl xs) of
    Just (linkUrl, afterUrl) ->
      case afterUrl of
        ('(':afterLParen) -> case (readLinkText afterLParen) of
          Just (linkText, remaining) -> Just ("<a href=\"" ++ linkUrl ++ "\">" ++ linkText ++ "</a>", remaining)
          Nothing -> Nothing
        otherwise -> Nothing
    Nothing -> Nothing
