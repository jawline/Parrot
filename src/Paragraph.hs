module Paragraph where
import Util

transformParagraphInt :: String -> (String, String)
transformParagraphInt xs = (xs, [])

transformParagraph :: String -> (String, String)
transformParagraph source = ("<p>" ++ (trim paragraph) ++ "</p>", remaining)
  where
    (paragraph, remaining) = (transformParagraphInt source)
