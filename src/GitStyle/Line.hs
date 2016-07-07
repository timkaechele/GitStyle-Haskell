module GitStyle.Line where

  import qualified GitStyle.Common as Common
  import qualified Data.Text as T
  import qualified Data.Char as Char

  data Line = Line T.Text
              deriving (Show, Eq)

  type Lines = [Line]

  -- ^Returns the text of a line
  text :: Line -> T.Text
  text (Line t) = t

  -- ^Calculates the length (char count) of a line
  length :: Line -> Int
  length = T.length . text

  {-|
    Checks if the line is part of an enumeration
    Enumerations start with '-' or '*'
  -}
  isEnumeration :: Line -> Bool
  isEnumeration l = elem firstChar enumerations
                      where
                        firstChar = (T.head . T.strip . text) l
                        enumerations = ['-', '*']

  {-|
    Strips the line from trailing whitespace and checks
    if the resulting line is empty.
  -}
  isBlank :: Line -> Bool
  isBlank l = strippedText l == T.empty
              where
                strippedText = T.strip . text

  {-|
    Checks if the line starts with an upper case letter
  -}
  startsWithUpperCase :: Line -> Bool
  startsWithUpperCase = Char.isUpper . T.head . text

  {-|
    Checks if the line ends with a dot '.'
  -}
  endsWithDot :: Line -> Bool
  endsWithDot = (==) '.' . T.last . text
