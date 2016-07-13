module GitStyle.CommitMessage(CommitMessage,
                              pack,
                              isValid,
                              isMultiLine,
                              subject,
                              body,
                              makeValid,
                              makeInvalid,
                              lineifyErrors,
                              printableCommit,) where

  import qualified GitStyle.Line as L
  import qualified GitStyle.Error as E
  import qualified Data.Text as T
  import qualified Data.List as List

  data CommitMessage = CommitMessage L.Lines
                     | ValidatableCommitMessage L.Lines
                     | ValidCommitMessage L.Lines
                     | InValidCommitMessage L.Lines E.Errors
                     deriving (Show, Eq)

  pack :: String -> CommitMessage
  pack s = if force then
              CommitMessage (L.replaceFirstLine l sanitizedFirstLine)
            else
              ValidatableCommitMessage l
           where
            l = (map L.pack . lines) s
            force = (L.containsWord forceText . head) l
            sanitizedFirstLine = (L.removeFromLine forceText . head) l

  forceText :: T.Text
  forceText = T.pack "!!FORCE!!"

  isValidatable :: CommitMessage -> Bool
  isValidatable (ValidatableCommitMessage _) = True
  isValidatable _ = False

  isValid :: CommitMessage -> Bool
  isValid (ValidCommitMessage _) = True
  isValid (CommitMessage _) = True
  isValid _ = False

  isMultiLine :: CommitMessage -> Bool
  isMultiLine = (<) 1 . length . getLines

  subject :: CommitMessage -> L.Line
  subject = head . getLines

  body :: CommitMessage -> L.Lines
  body = drop 1 . getLines

  getLines :: CommitMessage -> L.Lines
  getLines (CommitMessage l)            = l
  getLines (ValidatableCommitMessage l) = l
  getLines (ValidCommitMessage l)       = l
  getLines (InValidCommitMessage l _)   = l

  makeValid :: CommitMessage -> CommitMessage
  makeValid (ValidatableCommitMessage l) = ValidCommitMessage l
  makeValid c = c

  makeInvalid :: CommitMessage -> E.Errors -> CommitMessage
  makeInvalid (ValidatableCommitMessage l) e = InValidCommitMessage l e
  makeInvalid c _ = c

  getErrors :: CommitMessage -> E.Errors
  getErrors (InValidCommitMessage _ e) = e
  getErrors _ = []

  {-|
    Transforms the errors of the commit message to lines
    The resulting lines are formatted as an enumeration.

    printableErrors e -- ["- this is an error"]
  -}
  lineifyErrors :: CommitMessage -> L.Lines
  lineifyErrors c
                  | isValid c = [L.pack "Everythings fine."]
                  | otherwise = (map (L.toEnumLine . E.toLine) . getErrors) c

  printableCommit :: CommitMessage -> T.Text
  printableCommit c = (foldl T.append (T.pack "")) l
                      where
                        e = map L.commentate (lineifyErrors c)
                        l = map (L.printableLine) (getLines c ++ e)

