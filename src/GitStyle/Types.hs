module GitStyle.Types where

  import qualified Data.Text as T
  import qualified Data.Map as M


  {-|
    Represents a Line Info the first value is the line and the
    second value the column

    Counting should start at 1
  -}
  data LineInfo = LineInfo Int Int
                    deriving (Show, Eq)

  data TranslationKey = TranslationKey T.Text
                          deriving (Show, Eq, Ord)

  data Translations = Translations (M.Map TranslationKey T.Text)
                      deriving (Show, Eq)
