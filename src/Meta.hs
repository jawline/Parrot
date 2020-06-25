module Meta where
import Util

extractTitle :: String -> String
extractTitle source = trim (drop (length prelude) (findLine prelude source))
  where
    prelude = "!=!=! Title:"

extractDate :: String -> Float
extractDate source = read (trim (drop (length prelude) (findLine prelude source))) :: Float
  where
    prelude = "!=!=! Created:"

parseTags :: String -> String -> [String]
parseTags current [] = [current]
parseTags current (',':xs) = current:(parseTags "" xs)
parseTags current (x:xs) = parseTags (x:current) xs

extractTags :: String -> [String]
extractTags source = map trim $ map reverse $ tags 
  where
    prelude = "!=!=! Tags:"
    tags = parseTags "" (trim (drop (length prelude) (findLine prelude source)))

extractIntroduction :: String -> String
extractIntroduction source = trim intro
  where
    comment = "!=!=!"
    start = comment ++ " Intro: Start"
    end = comment ++ " Intro: End"
    portion = untilString "!=!=! Intro: End" (fromString "!=!=! Intro: Start" source)
    intro = drop (length start) (reverse (drop (length end) (reverse portion))) 

mergeTag :: String -> String -> String
mergeTag next current = if (length current) == 0 then next else current ++ ", " ++ next
mergeTags tags = foldr mergeTag "" tags

type ArticleInfo = (String, String, Float, [String])
extractMetadata :: String -> ArticleInfo
extractMetadata source = (title, info, date, tags)
  where
    title = extractTitle source
    info = extractIntroduction source
    date = extractDate source
    tags = extractTags source
        
