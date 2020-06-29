module ImageTemplates where
import System.FilePath
import System.Directory

data ExpectedSize = Original
  | Fixed Float Float
  | Scaled Float
  | ScaledToWidth Float
  | ScaledToHeight Float
  deriving Show

data ImageExpectation = ImageExpectation {
  name :: String,
  expected :: ExpectedSize
}

type ImageExpectations = [String]
type Images = [FilePath]

transformImages :: FilePath -> ImageExpectations -> IO ()
transformImages = do
  putStrLn ("[+] Transforming Images")
  return ()
