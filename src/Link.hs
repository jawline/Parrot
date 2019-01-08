module Link where

isStartOfLink :: Char -> Bool
isStartOfLink '[' = True
isStartOfLink n = False

readToNext :: String -> Char -> Maybe (String, String)
readToNext [] _ = Nothing 
readToNext (x:xs) y
  | x == y    = Just ([], xs)
  | otherwise = case (readToNext xs y) of
      Just (part1, remaining) -> Just (x:part1, remaining)
      Nothing -> Nothing

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
