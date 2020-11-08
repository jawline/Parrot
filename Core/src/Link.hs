module Link where
import Util

isStartOfLink :: String -> Maybe String
isStartOfLink ('[':xs) = Just xs
isStartOfLink _ = Nothing

isStartOfImage :: String -> Maybe String
isStartOfImage xs
  | ('!':'[':xs) <- xs = Just xs
  | otherwise = Nothing

readLinkText xs = (readToNext xs (\(x:_) -> x == ']'))
readLinkUrl xs = (readToNext xs (\(x:_) -> x == ')'))

internalLinkTransformer rewriter xs
  | Just (linkText, afterUrl) <- readLinkText xs,
    ('(':afterLParen) <- afterUrl,
    Just (linkUrl, remaining) <- readLinkUrl (afterLParen)
  = Just (rewriter linkText linkUrl, remaining)
  | otherwise = Nothing

transformLinkInt = internalLinkTransformer (\linkText linkUrl -> "<a href=\"" ++ linkUrl ++ "\">" ++ linkText ++ "</a>")
transformImageInt = internalLinkTransformer (\linkText linkUrl -> "<img src=\"" ++ linkUrl ++ "\"/>")

{-| Transform images ![Description]{URL} and links [Description test]{URL}
from markdown to HTML.

If head of string is a link then return the translated link + the remaining unprocessed markdown, otherwise return Nothing.
-}
transformLink :: String -> Maybe (String, String)
transformLink xs
  | ('!':'[':startData) <- xs = transformImageInt startData
  | ('[':startData) <- xs = transformLinkInt startData
  | otherwise = Nothing
