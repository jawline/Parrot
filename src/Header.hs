module Header where
import Util

transformHeaderInt :: String -> (String, String)
transformHeaderInt [] = ([], [])
transformHeaderInt ('\n':xs) = ([], xs)
transformHeaderInt (x:xs) = ((x:rest), remaining)
  where
    (rest, remaining) = (transformHeaderInt xs)

transformHeaderCountLevel :: String -> (Int, String)
transformHeaderCountLevel [] = (0, [])
transformHeaderCountLevel ('#':xs) = (level + 1, rest)
  where
    (level, rest) = (transformHeaderCountLevel xs)
transformHeaderCountLevel xs = (0, xs)

transformHeader :: String -> (String, String)
transformHeader source = ("<h" ++ (show level) ++ ">" ++ header ++ "</h" ++ (show level) ++ ">", remaining)
  where
    (header, remaining) = (transformHeaderInt (trimWhiteLine headerStart))
    (level, headerStart) = (transformHeaderCountLevel source)


