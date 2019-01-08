module Paragraph where
import Util
import Link
import Code

isParagraphEnd :: Char -> Bool
isParagraphEnd '\n' = True
isParagraphEnd n = False

combine :: (String, String) -> (String, String) -> (String, String)
combine (l1, r1) (l2, r2) = (l1 ++ l2, r2)

transformParagraphInt :: String -> (String, String)
transformParagraphInt [] = ([], [])
transformParagraphInt (x:xs)
  | isParagraphEnd x = ([], xs)
  | isInlineCodeStart x = case (transformInlineCode xs) of
    Just d -> combine d (transformParagraphInt remaining)
    Nothing -> combine ([x], xs) (transformParagraphInt xs)
  | isStartOfLink x = case (transformLink xs) of
    Just d -> combine d (transformParagraphInt remaining)
    Nothing -> combine ([x], xs) (transformParagraphInt xs)
  | otherwise = (x:paragraph, remaining)
    where
      (paragraph, remaining) = (transformParagraphInt xs)

transformParagraph :: String -> (String, String)
transformParagraph source = ("<p>" ++ (trim paragraph) ++ "</p>", remaining)
  where
    (paragraph, remaining) = (transformParagraphInt source)
