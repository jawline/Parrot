{-# LANGUAGE QuasiQuotes #-}
import Text.RawString.QQ
import Data.Time (formatTime, getCurrentTime, defaultTimeLocale)
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(ExitFailure))
import Control.Monad (when)

articleTemplate :: String -> String
articleTemplate currentDate = [r|!=!=! Title: Article Title
!=!=! Created: |] ++ currentDate ++ [r|
!=!=! Tags: List1, List2, etc

!=!=! Intro: Start
Put the description / introduction of your article here.
!=!=! Intro: End

Put the main body of content here.|]

failArguments = do
  putStrLn "Usage: executable OUTPUT_ARTICLE_FILEPATH"
  exitWith (ExitFailure 1)

main = do
  programArguments <- getArgs

  when (length programArguments /= 1) failArguments
  let targetFile = programArguments !! 0

  createdAt <- getCurrentTime
  let humanReadableTime = (formatTime defaultTimeLocale "%d-%m-%Y" createdAt)
  let articleTemplate' = articleTemplate humanReadableTime

  writeFile targetFile articleTemplate'
  putStrLn ("[+] Wrote " ++ targetFile ++ ". Now open this file in your text editor.")
  return ()
