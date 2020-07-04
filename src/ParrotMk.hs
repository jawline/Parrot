{-# LANGUAGE QuasiQuotes #-}
import Text.RawString.QQ
import Data.Time

multiline :: String
multiline = [r|!=!=! Title: ${{{ARTICLE_TITLE}}}
!=!=! Created: ${{{CREATED_ON}}}
!=!=! Tags: ${{{ARTICLE_TAGS}}}

!=!=! Intro: Start
Put the description / introduction of your article here.
!=!=! Intro: End

Put the main body of content here.
|]

main = do
  createdAt <- getCurrentTime
  putStrLn (show createdAt)
  return ()
