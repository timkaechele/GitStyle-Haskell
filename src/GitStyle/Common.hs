module GitStyle.Common where
  import qualified Data.Text as T

  firstWord :: T.Text -> T.Text
  firstWord = head . T.words

  usesImperative :: T.Text -> Bool
  usesImperative = not . T.isSuffixOf indicative . firstWord
                    where
                      indicative = T.pack "ed"
