import System.FSNotify (withManager, watchTree)
import System.Environment (getArgs)
import Control.Concurrent (threadDelay)
import Control.Monad (forever, when)
import System.Exit (exitWith, ExitCode(ExitFailure))
import System.Process (system)

-- Triggers cabal to recompile on change and then re-run the website builder.
executeParrot input output = do
  system ("cabal run Parrot -- " ++ input ++ " " ++ output)
  return ()

-- Repeatedly watches input or ./src/ and runs the transformer whenever files change.
watchJob transformer input output =
  withManager $ \mgr -> do
    putStrLn ("[+] Watching for changes on: " ++ input)
    watchTree mgr input alwaysTrigger triggerTransform
    watchTree mgr "src" alwaysTrigger triggerTransform
    forever $ threadDelay 1000000
  where
    alwaysTrigger _ = True
    triggerTransform event = do
      putStrLn ("[+] Need to transform because of " ++ (show event))
      transformer
      putStrLn ("[+] Transform because of change to " ++ (show event))
      return ()

-- When the watcher is not called with the right number of arguments complain
failArguments = do
  putStrLn "Usage: executable SOURCE_DIR OUTPUT"
  exitWith (ExitFailure 1)

{-|
 The purpose of this program is to watch for changes in either the project
 source or the target directory and automatically build a copy of the
 website if files in either change. This is primarily a development tool
 to enable rapid prototyping both in changes to the target site but also in
 Parrot itself.
-}
main = do
  programArguments <- getArgs

  when (length programArguments /= 2) $ failArguments

  let input = programArguments !! 0
  let output = programArguments !! 1
  let exec = executeParrot input output

  _ <- exec
  watchJob exec input output
  return ()
