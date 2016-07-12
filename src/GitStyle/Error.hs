module GitStyle.Error(Error(..),
                      Errors(..),
                      toLine
                      ) where

  import qualified GitStyle.Line as L
  import qualified Data.List as List

  type LineNumber = Int

  data Error = SubjectTrailingDot
             | SubjectLength
             | SubjectNoUpperCase
             | SubjectIndicative
             | BodyNoEmptyLine
             | BodyLength [LineNumber]
             deriving (Eq)

  type Errors = [Error]

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
                                    lines = formatList l

  formatList :: Show a => [a] -> String
  formatList = List.intercalate ", " . map show

  toLine :: Error -> L.Line
  toLine = L.pack . errorMessage
