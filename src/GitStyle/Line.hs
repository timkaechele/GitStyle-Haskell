module GitStyle.Line(Line,
                     Lines(..),
                     pack,
                     lineLength,
                     endsWith,
                     endsWithDot,
                     isImperative,
                     isBlank,
                     startsWithUpperCase,
                     containsWord,
                     removeFromLine,
                     replaceFirstLine,
                     commentate,
                     toEnumLine
                     ) where

  import qualified Data.Text as T
  import qualified Data.Char as C
  import Data.Maybe

  data Line = EmptyLine
            | Line T.Text
            deriving (Eq)

  type Lines = [Line]

  instance Show Line where
    show = show . getText


  {-|
    Wraps the given text in a line.
  -}
  packFromText :: T.Text -> Line
  packFromText t
               | text == T.empty = EmptyLine
               | otherwise       = Line text
               where
                text = T.strip t

  {-|
    Wraps the given string in a line.
  -}
  pack :: String -> Line
  pack s = packFromText (T.pack s)

  {-|
    Returns the line length (character count) of the line
  -}
  lineLength :: Line -> Int
  lineLength = T.length . getText

  {-|
    Checks if the given line is blank (no characters
    or only whitespace)
  -}
  isBlank :: Line -> Bool
  isBlank EmptyLine = True
  isBlank _ = False

  {-|
    Returns the text of the line.
    If the line is empty you get an empty text.
  -}
  getText :: Line -> T.Text
  getText EmptyLine = T.empty
  getText (Line t) = t

  endsWith :: T.Text -> Line -> Bool
  endsWith _ EmptyLine = False
  endsWith t l = (T.isSuffixOf t . getText) l

  {-|
    Checks if the given line ends with a dot (".").
  -}
  endsWithDot :: Line -> Bool
  endsWithDot = endsWith (T.pack ".")

  {-|
    If the line is empty Nothing will be returned,
    otherwise just the first word.
  -}
  firstWord :: Line -> Maybe T.Text
  firstWord EmptyLine = Nothing
  firstWord l = (Just . head . T.words . getText) l

  {-|
    Checks if a given word is contained in the line
  -}
  containsWord :: T.Text -> Line -> Bool
  containsWord _ EmptyLine = False
  containsWord w l = (T.isInfixOf w . getText) l

  {-|
    Inspects the first word of the line and checks if it
    uses an imperative mood.
  -}
  isImperative :: Line -> Bool
  isImperative = not . indicative . fromMaybe (T.pack "") . firstWord
                  where
                    indicative = T.isSuffixOf (T.pack "ed")

  {-|
    Checks if the line starts with an upper case character
  -}
  startsWithUpperCase :: Line -> Bool
  startsWithUpperCase l
                      | w == Nothing = False
                      | otherwise    = (C.isUpper . T.head . fromJust) w
                        where
                          w = firstWord l


  {-|
    Removes the given text from the line
  -}
  removeFromLine :: T.Text -> Line -> Line
  removeFromLine _ EmptyLine = EmptyLine
  removeFromLine t l = (packFromText . cleanup . replace . getText) l
                      where
                        cleanup = T.unwords . T.words
                        replace = T.replace t T.empty

  {-|
    Takes a list of lines and line
    and replaces the first line in the list with
    the given line
  -}
  replaceFirstLine :: Lines -> Line -> Lines
  replaceFirstLine [] _ = []
  replaceFirstLine (x:xs) l = l : xs

  {-|
    Adds the given text to the front of the line.
    This is an O(n) operation due to array like
    character of line.
  -}
  addFront :: T.Text -> Line -> Line
  addFront t EmptyLine = packFromText t
  addFront t l = (packFromText . T.concat) [t, getText l]

  {-|
    Commentates the given line

    let l = pack "Hello World"
    commentate l -- "# Hello World"
  -}
  commentate :: Line -> Line
  commentate = addFront (T.pack "# ")

  {-|
    Adds an enumeration sign to the line

    let l = pack "Hello World"
    toEnumLine l -- "- Hello World"
  -}
  toEnumLine :: Line -> Line
  toEnumLine EmptyLine = EmptyLine
  toEnumLine l = addFront (T.pack "- ") l

  printableLine :: Line -> String
  printableLine EmptyLine = "\n"
  printableLine (Line t) = (show t) ++ "\n"
