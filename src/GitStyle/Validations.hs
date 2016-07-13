module GitStyle.Validations where
  import Data.Maybe
  import Data.List (findIndices)

  import qualified GitStyle.Line as L
  import qualified GitStyle.Error as E
  import qualified GitStyle.CommitMessage as C

  type Validator = (C.CommitMessage -> Maybe E.Error)

  data Validation = Validation [Validator] C.CommitMessage

  validate :: Validation -> C.CommitMessage
  validate (Validation v c)
            | hasErrors = C.makeInvalid c result
            | otherwise = C.makeValid c
            where
              result = (catMaybes . map ($ c)) v
              hasErrors = (not . null) result

  {-|
    Checks that the subject line doesn't end with a dot.
  -}
  subjectNoDot :: C.CommitMessage -> Maybe E.Error
  subjectNoDot c
               | (L.endsWithDot . C.subject) c = Just E.SubjectTrailingDot
               | otherwise                     = Nothing


  {-|
    Checks that the subject line doesn't exceed a length of 50 characters.
  -}
  subjectLength :: C.CommitMessage -> Maybe E.Error
  subjectLength c
             | not correctSubjectLength = Just E.SubjectLength
             | otherwise            = Nothing
              where
                correctSubjectLength = ((>=) 50 . L.lineLength . C.subject) c


  {-|
    Checks that the first word of the commit is in an imperative mood.

    It does so by validating that the first word doesn't use the indicative
    mood.
  -}
  subjectImperative :: C.CommitMessage -> Maybe E.Error
  subjectImperative c
             | not imperative = Just E.SubjectIndicative
             | otherwise      = Nothing
              where
                imperative = (L.isImperative . C.subject) c

  {-|
    Checks that the subject starts with an upper case character.
  -}
  subjectUpperCase :: C.CommitMessage -> Maybe E.Error
  subjectUpperCase c
             | (not . isUpperCase) c = Just E.SubjectNoUpperCase
             | otherwise       = Nothing
              where
                isUpperCase = (L.startsWithUpperCase . C.subject)

  {-|
    Checks that the CommitMessage body starts with an empty line.
  -}
  bodyEmptyLine :: C.CommitMessage -> Maybe E.Error
  bodyEmptyLine c
                | (not . C.isMultiLine) c = Nothing
                | (not . L.isBlank . head . C.body) c = Just E.BodyNoEmptyLine
                | otherwise               = Nothing


  {-|
    Validates the line lengths of the body.
  -}
  bodyLength :: C.CommitMessage -> Maybe E.Error
  bodyLength c
             | hasLongLines = Just (E.BodyLength humanLines)
             | otherwise    = Nothing
             where
              longLines = (findIndices ((<) 72 . L.lineLength) . C.body) c
              hasLongLines = (not . null) longLines
              humanLines = map ((+) 1) longLines

  {-|
    Validate a given commit message with the rules defined by chris beams
    git style guide.
  -}
  chrisBeamsValidate :: C.CommitMessage -> C.CommitMessage
  chrisBeamsValidate = validate . Validation validations
                        where
                          validations = [
                            subjectNoDot,
                            subjectLength,
                            subjectImperative,
                            subjectUpperCase,
                            bodyEmptyLine,
                            bodyLength]
