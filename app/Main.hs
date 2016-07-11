module Main where
  import System.Environment
  import GitStyle.CommitMessage
  import GitStyle.Common
  import GitStyle.Executable
  import GitStyle.Error
  import System.IO
  import System.FilePath.Posix
  import System.Directory
  import System.Exit

  main :: IO ()
  main = do
    args <- getArgs
    gitDirectory <- getCurrentDirectory
    let commitMessagePath = joinPath [gitDirectory, head args]
    rawCommitMessage <- readFile commitMessagePath
    let commitMessage =  validateRawCommitMessage rawCommitMessage
    putStrLn $ (foldl (++) "" . showErrors) commitMessage
    exitWith (ExitFailure 1)
