module Link where

isStartOfLink :: Char -> Bool
isStartOfLink '[' = True
isStartOfLink n = False

readToNext :: String -> Char -> Maybe (String, String)
readToNext [] _ = Nothing 
readToNext (x:xs) y
  | x == y    = Just ([], xs)
  | otherwise = Just (x:part1, remaining)
    where
      Just (part1, remaining) = (readToNext xs y)

readLinkUrl  xs = (readToNext xs ']')
readLinkText xs = (readToNext xs ')')

transformLink :: String -> Maybe (String, String)
transformLink xs = Just ("<a href=\"" ++ linkUrl ++ "\">" ++ linkText ++ "</a>", remaining)
  where
    Just (linkUrl, afterUrl) = (readLinkUrl xs)
    Just (linkText, remaining) = (readLinkText (tail afterUrl))
