module GitStyle.CommitMessage where

  import qualified Data.List as L
  import qualified Data.Text as T
  import GitStyle.Line
  import GitStyle.Error
  import Data.Maybe


  data CommitMessage = CommitMessage Lines -- unprocessed Commit Message
                     | InvalidCommitMessage Lines Errors
                     | ValidCommitMessage Lines
                       deriving (Eq)

  instance Show CommitMessage where
    show = L.concat . map show . getLines

  {-|
    Returns True if the CommitMessage has multiple lines
  -}
  isMultiLine :: CommitMessage -> Bool
  isMultiLine = (<) 1 . length . getLines

  {-|
    Checks if the subject line contains the string "!!FORCE!!"
    If so the function returns false.
  -}
  isValidatable :: CommitMessage -> Bool
  isValidatable = not . T.isInfixOf force . text . subject


  force :: T.Text
  force = T.pack "!!FORCE!!"

  sanitize :: CommitMessage -> CommitMessage
  sanitize c = CommitMessage $ (sanitizedSubject c) : (body c)
                where
                  sanitizedSubject = (removeFromLine force . subject)

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
  getLines (InvalidCommitMessage l _) = l
  getLines (ValidCommitMessage l) = l

  getErrors :: CommitMessage -> Maybe Errors
  getErrors (InvalidCommitMessage _ e) = Just e
  getErrors _ = Nothing

  {-|
    Builds a CommitMessage from the given String
  -}
  buildFromString :: String -> CommitMessage
  buildFromString s = CommitMessage lines
                        where
                          text = T.pack s
                          lines = map (Line) (T.lines text)
