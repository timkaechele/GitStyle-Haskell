module GitStyle.CommitMessage where

  import qualified Data.Text as T
  import GitStyle.Line

  data CommitMessage = CommitMessage Lines
                       deriving (Show, Eq)

  {-|
    Returns True if the CommitMessage has multiple lines
  -}
  isMultiLine :: CommitMessage -> Bool
  isMultiLine = (<) 1 . length . getLines

  {-|
    Checks if the string "!!FORCE!!" is in the subject line.
    If so the function returns false.
  -}
  isValidatable :: CommitMessage -> Bool
  isValidatable = T.isInfixOf (T.pack "!!FORCE!!") . text . subject


  sanitize :: CommitMessage -> CommitMessage

  {-|
    Extracts the subject line from the CommitMessage
  -}
  subject :: CommitMessage -> Line
  subject = head . getLines

  {-|
    Extracts the body lines from the CommitMessage
  -}
  body :: CommitMessage -> Lines
  body = drop 1 . getLines

  {-|
    Returns all lines from the CommitMessage
  -}
  getLines :: CommitMessage -> Lines
  getLines (CommitMessage l) = l

  {-|
    Builds a CommitMessage from the given String
  -}
  buildFromString :: String -> CommitMessage
  buildFromString s = CommitMessage lines
                        where
                          text = T.pack s
                          lines = map (Line) (T.lines text)
