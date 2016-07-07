module GitStyle.Validations where

{-
  import qualified Data.Text as T
  import qualified GitStyle.Types as Types
  import qualified GitStyle.Commit as Commit
  import qualified GitStyle.Line as Line
  import Data.Maybe

  validateFirstLineLength :: Types.CommitMessage -> Maybe Error
  validateFirstLineLength c
                          | lineLength > 50 = Error (T.pack "")
                            where
                                lineLength = Line.length Commit.firstLine c
                                errorMessage =
-}
