module Code where
import Util

isInlineCodeStart :: Char -> Bool
isInlineCodeStart c = c == '`'

transformInlineCodeInt :: String -> Maybe (String, String)
transformInlineCodeInt xs = (readToNext xs (\x -> x == '`')) 

transformInlineCode :: String -> Maybe (String, String)
transformInlineCode xs =
  case (transformInlineCodeInt xs) of
    Just (line, remaining) -> Just ("<code>" ++ xs ++ "</code>", remaining)
    Nothing -> Nothing
