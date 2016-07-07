module GitStyle.Test.Line where

  import GitStyle.Line
  import qualified Data.Text as T
  import Test.HUnit

  blankLine :: Line
  blankLine = Line (T.pack "  ")

  testIsBlank = TestCase $
                      assertBool "Line should be blank" (isBlank blankLine)



