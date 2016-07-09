module GitStyle.Line where

  import GitStyle.Common
  import qualified Data.Text as T
  import qualified Data.Char as Char

  data Line = Line T.Text
              deriving (Show, Eq)

  type Lines = [Line]

  {-|
    Returns the text of a line
  -}
  text :: Line -> T.Text
  text (Line t) = t

  {-|
    Calculates the length (char count) of a line
  -}
  textLength :: Line -> Int
  textLength = T.length . text

  {-|
    Checks if the line is part of an enumeration
    Enumerations start with '-' or '*'
  -}
  isEnumeration :: Line -> Bool
  isEnumeration l = elem (firstChar l) enumerations

  {-|
    Returns the first non whitespace char from the line
  -}
  firstChar :: Line -> Char
  firstChar = T.head . T.strip . text

  {-|
    A list with all enumeration chars
  -}
  enumerations :: [Char]
  enumerations = ['-', '*']

  {-|
    Removes enumeration chars from the line
  -}
  sanitize :: Line -> Line
  sanitize l
           | isEnumeration l = Line (sanitize' l)
           | otherwise = l
           where
            sanitize' = T.strip . T.drop 1 . T.strip . text

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
  startsWithUpperCase = Char.isUpper . firstChar

  {-|
    Checks if the line ends with a dot '.'
  -}
  endsWithDot :: Line -> Bool
  endsWithDot = (==) '.' . T.last . text

  {-|
    Returns a list with the line lengths of the given lines
  -}
  lineLengths :: Lines -> [Int]
  lineLengths = map textLength

  removeFromLine :: T.Text -> Line -> Line
  removeFromLine w = Line . normalizeSpacing . delete . text
                      where
                        delete = (T.replace w T.empty)
                        normalizeSpacing = T.strip . T.unwords . T.words
  {-|
    Filters lines that meat a certain criteria and returns the line numbers
    of those lines.
    The first argument is the initial line number.
  -}
  filterLines :: Int -> ((Int, Line) -> Bool) -> Lines -> [Int]
  filterLines s f = (map fst . filter f . withCount s)
