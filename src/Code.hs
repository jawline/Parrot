module Code where
import Util

isInlineCodeStart :: Char -> Bool
isInlineCodeStart c = c == '`'

isInlineCodeEnd :: String -> Bool
isInlineCodeEnd ('`':xs) = True
isInlineCodeEnd _ = False

transformInlineCodeInt :: String -> Maybe (String, String)
transformInlineCodeInt xs = readToNext xs isInlineCodeEnd 

transformInlineCode :: String -> (String, String)
transformInlineCode xs = case (transformInlineCodeInt xs) of
  Just (line, remaining) -> ("<span><code>" ++ line ++ "</code></span>", remaining)
  Nothing -> error "No end to inline block"

isCodeEnd :: String -> Bool
isCodeEnd ('`':'`':'`':xs) = True
isCodeEnd _ = False

transformMultilineCode :: String -> (String, String)
transformMultilineCode xs = case readToNext xs isCodeEnd of
  Just (code, rest) -> ("<pre><code>" ++ code ++ "</code></pre>", drop 3 rest)
  Nothing -> error "No end to multiline block"
