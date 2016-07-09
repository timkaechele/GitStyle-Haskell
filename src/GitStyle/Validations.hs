module GitStyle.Validations where

  import GitStyle.Common
  import GitStyle.Line
  import GitStyle.CommitMessage
  import qualified GitStyle.Error as E
  import qualified Data.Text as T
  import Data.Maybe

  type Validator = (CommitMessage -> Maybe E.Error)

  data Validation = Validation [Validator] CommitMessage

  validate :: Validation -> [E.Error]
  validate (Validation v c) = (catMaybes . map ($ c)) v

  subjectNoDot :: CommitMessage -> Maybe E.Error
  subjectNoDot c
               | (endsWithDot . subject) c = Just E.SubjectTrailingDot
               | otherwise                 = Nothing

  subjectLength :: CommitMessage -> Maybe E.Error
  subjectLength c
             | correctSubjectLength = Just E.SubjectLength
             | otherwise            = Nothing
              where
                correctSubjectLength = (textLength . subject) c > 50

  subjectImperative :: CommitMessage -> Maybe E.Error
  subjectImperative c
             | not isImperative = Just E.SubjectIndicative
             | otherwise        = Nothing
              where
                isImperative = (usesImperative . text . subject) c

  subjectUpperCase :: CommitMessage -> Maybe E.Error
  subjectUpperCase c
             | (not . isUpperCase) c = Just E.SubjectNoUpperCase
             | otherwise       = Nothing
              where
                isUpperCase = (startsWithUpperCase . subject)

  bodyEmptyLine :: CommitMessage -> Maybe E.Error
  bodyEmptyLine c
                | (not . isMultiLine) c           = Nothing
                | (not . isBlank . head . body) c = Just E.BodyNoEmptyLine
                | otherwise                       = Nothing

  bodyLength :: CommitMessage -> Maybe E.Error
  bodyLength c
             | hasLongLines c = Just (E.BodyLength (longLines c))
             | otherwise      = Nothing
             where
              longLines = filterLines 0 (\(p, l) -> textLength l > 72) . body
              hasLongLines = (not . null . longLines)

  chrisBeamsValidate :: CommitMessage -> [E.Error]
  chrisBeamsValidate = validate . Validation validations
                        where
                          validations = [
                            subjectNoDot,
                            subjectLength,
                            subjectImperative,
                            subjectUpperCase,
                            bodyEmptyLine,
                            bodyLength]
