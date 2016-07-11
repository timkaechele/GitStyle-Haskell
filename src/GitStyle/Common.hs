module GitStyle.Common where
  import qualified Data.Text as T
  import qualified Data.List as L

  {-|
    Returns the first word of a given text
  -}
  firstWord :: T.Text -> T.Text
  firstWord = head . T.words

  {-|
    Checks if the first word in the text uses the imperative mood.
  -}
  usesImperative :: T.Text -> Bool
  usesImperative = not . T.isSuffixOf indicative . firstWord
                    where
                      indicative = T.pack "ed"

  {-|
    Formats a given list of showable data structures.
    The output looks something like this
    showList [1,2,3] = "1, 2, 3"
  -}
  formatList :: (Show a) => [a] -> T.Text
  formatList = T.pack . L.intercalate ", ". (map show)

  {-|
    Adds a dotfile comment sign ('#') in front of the given strings
  -}
  commentate :: [String] -> [String]
  commentate = map ((++) "# ")

  {-|
    Accepts a starting position and a list and returns
    tuples with the starting position and the list item

    withCount 0 ['a', 'b', 'c'] = [(0, 'a'), (1, 'b'), (2, 'c')]
  -}
  withCount :: Int -> [a] -> [(Int, a)]
  withCount p = zip [p..]

