module GitStyle.Error where

  type LineNumber = Int

  data Error = SubjectTrailingDot
             | SubjectLength
             | SubjectNoUpperCase
             | SubjectIndicative
             | BodyNoEmptyLine
             | LineLength LineNumber
             | BodyIndicative LineNumber
             deriving (Eq)

  instance Show Error where
    show = errorMessage


  errorMessage :: Error -> String
  errorMessage _ = "It's an error"
