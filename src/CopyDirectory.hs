module CopyDirectory where

import System.Directory
import System.FilePath((</>))
import Control.Applicative((<$>))
import Control.Monad(when,forM_)

copyDirectory ::  FilePath -> FilePath -> IO ()
copyDirectory src dst = do
  existingDirectory <- doesDirectoryExist dst
  when (existingDirectory == False) $ createDirectory dst
  content <- listDirectory src 
  forM_ content $ \name -> do
    let srcPath = src </> name
    let dstPath = dst </> name
    isDirectory <- doesDirectoryExist srcPath
    if isDirectory
      then copyDirectory srcPath dstPath
      else copyFile srcPath dstPath
