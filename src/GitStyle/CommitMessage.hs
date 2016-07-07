module GitStyle.CommitMessage where

  import qualified Data.Text as T
  import qualified Prelude as P
  import GitStyle.Line

  data CommitMessage = CommitMessage Lines
                       deriving (P.Show, P.Eq)

  isMultiLine :: CommitMessage -> P.Bool
  isMultiLine (CommitMessage l) = P.length l P.> 1


  subject :: CommitMessage -> Line
  subject = P.head . getLines

  body :: CommitMessage -> Maybe Line

  lines :: CommitMessage -> Lines
  getLines (CommitMessage l) = l

  buildFromString :: P.String -> CommitMessage
  buildFromString s = CommitMessage lines
                        where
                          text = T.pack s
                          lines = P.map (Line) (T.lines text)
