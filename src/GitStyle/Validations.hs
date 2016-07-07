module GitStyle.Validations where

  import GitStyle.Line
  import GitStyle.CommitMessage
  import qualified GitStyle.Error as E
  import qualified Data.Text as T
  import Data.Maybe

  type Validator = (CommitMessage -> Maybe Error)

  data Validation = Validation [Validator] CommitMessage
                    deriving (Show, Eq)


  validate :: Validation -> [Error]
  validate (Validation v c) = filter (/= Nothing) (map c v)

  noDot :: CommitMessage -> Maybe Error
  noDot c =
        | (endsWithDot . subject) c = Just E.SubjectTrailingDot
        | otherwise                 = Nothing

  subjectLength :: CommitMessage -> Maybe Error
  subjectLength c
             | correctSubjectLength = Just E.SubjectLength
             | otherwise         = Nothing
              where
                correctSubjectLength = (textLength . subject) c > 50

  subjectImperative :: CommitMessage -> Maybe Error
  subjectImperative c
             | not isImperative = Just E.SubjectImperative
             | otherwise        = Nothing
              where
                isImperative = (usesImperative . text . subject) c

  subjectCapitalized :: CommitMessage -> Maybe Error
  subjectCapitalized c
             | not isImperative = Just E.SubjectNoUpperCase
             | otherwise        = Nothing
              where
                isImperative = (usesImperative . text . subject) c


  -- bodyLength :: CommitMessage -> Maybe Error
  -- bodyLength c
  --            | 
