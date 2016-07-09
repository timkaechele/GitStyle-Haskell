module GitStyle.Error where

  import qualified Data.List as L

  type LineNumber = Int

  data Error = SubjectTrailingDot
             | SubjectLength
             | SubjectNoUpperCase
             | SubjectIndicative
             | BodyNoEmptyLine
             | BodyLength [LineNumber]
             deriving (Eq)

  {-|
    To show an error just call the error message
  -}
  instance Show Error where
    show = errorMessage

  {-|
    Maps a given Error to an human readable string.
  -}
  errorMessage :: Error -> String
  errorMessage SubjectTrailingDot = "The subject ends with a dot."
  errorMessage SubjectLength = "The subject is too long."
  errorMessage SubjectNoUpperCase = "The subject starts with a lowercase char."
  errorMessage SubjectIndicative = "The subject uses indicative mood."
  errorMessage BodyNoEmptyLine = "The body doesn't start with an empty line."
  errorMessage (BodyLength l) = "The body is too long in Line " ++ lines ++ "."
                                  where
                                    lines = (L.intercalate ", " . map show) l
