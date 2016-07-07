module GitStyle.Common where
  import qualified Data.Text as T

  firstWord :: T.Text -> T.Text
  firstWord = head . T.words

  isList :: T.Text -> Bool
  isList t = True

  usesImperative :: T.Text -> Bool
  usesImperative = T.isSuffixOf imperative . firstWord
                    where
                      imperative = T.pack "ed"
