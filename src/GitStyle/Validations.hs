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

  subjectNoDot :: C.CommitMessage -> Maybe E.Error
  subjectNoDot c
               | (L.endsWithDot . C.subject) c = Just E.SubjectTrailingDot
               | otherwise                     = Nothing

  subjectLength :: C.CommitMessage -> Maybe E.Error
  subjectLength c
             | not correctSubjectLength = Just E.SubjectLength
             | otherwise            = Nothing
              where
                correctSubjectLength = ((>=) 50 . L.lineLength . C.subject) c

  subjectImperative :: C.CommitMessage -> Maybe E.Error
  subjectImperative c
             | not imperative = Just E.SubjectIndicative
             | otherwise      = Nothing
              where
                imperative = (L.isImperative . C.subject) c

  subjectUpperCase :: C.CommitMessage -> Maybe E.Error
  subjectUpperCase c
             | (not . isUpperCase) c = Just E.SubjectNoUpperCase
             | otherwise       = Nothing
              where
                isUpperCase = (L.startsWithUpperCase . C.subject)

  bodyEmptyLine :: C.CommitMessage -> Maybe E.Error
  bodyEmptyLine c
                | (not . C.isMultiLine) c = Nothing
                | (not . L.isBlank . head . C.body) c = Just E.BodyNoEmptyLine
                | otherwise               = Nothing

  bodyLength :: C.CommitMessage -> Maybe E.Error
  bodyLength c
             | hasLongLines = Just (E.BodyLength humanLines)
             | otherwise    = Nothing
             where
              longLines = (findIndices ((<) 50 . L.lineLength) . C.body) c
              hasLongLines = (not . null) longLines
              humanLines = map ((+) 1) longLines

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
