module List where
import Util
import Paragraph

transformListInner [] = []
transformListInner xs = xr
  where xr = list ++ transformListInner remaining
        (list, remaining) = transformSpan xs

transformListLine :: String -> (String, String)
transformListLine xs = case readToNext xs (\(x:_) -> x == '\n') of
  Just (list, remaining) -> ("<li>" ++ (transformListInner list) ++ "</li>", remaining)
  Nothing -> ([], xs)

transformListInt :: String -> (String, String)
transformListInt ('*':xs) = (line ++ restlist, remaining)
  where (line, r1) = transformListLine xs 
        (restlist, remaining) = transformListInt r1
transformListInt xs = ([], xs)

transformList :: String -> (String, String) 
transformList xs = ("<ul>" ++ list ++ "</ul>", remaining)
  where (list, remaining) = transformListInt xs
