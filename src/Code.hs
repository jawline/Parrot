module Code where
import Util

isInlineCodeStart :: Char -> Bool
isInlineCodeStart c = c == '`'

isInlineCodeEnd :: String -> Bool
isInlineCodeEnd ('`':xs) = True
isInlineCodeEnd _ = False

transformInlineCodeInt :: String -> Maybe (String, String)
transformInlineCodeInt xs = readToNext xs isInlineCodeEnd 

transformInlineCode :: String -> Maybe (String, String)
transformInlineCode xs = case (transformInlineCodeInt xs) of
  Just (line, remaining) -> Just ("<span><code>" ++ line ++ "</code></span>", remaining)
  Nothing -> Nothing

isCodeEnd :: String -> Bool
isCodeEnd ('`':'`':'`':xs) = True
isCodeEnd _ = False

transformMultilineCode :: String -> Maybe (String, String)
transformMultilineCode xs = case readToNext xs isCodeEnd of
  Just (code, rest) -> Just ("<pre><code>" ++ code ++ "</code></pre>", drop 3 rest)
  Nothing -> Nothing
