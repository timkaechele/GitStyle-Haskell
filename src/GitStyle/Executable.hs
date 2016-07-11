module GitStyle.Executable where

  import GitStyle.CommitMessage
  import GitStyle.Validations
  import GitStyle.Error

  validateRawCommitMessage :: String -> CommitMessage
  validateRawCommitMessage = chrisBeamsValidate . buildFromString

  showErrors :: CommitMessage -> [String]
  showErrors (InvalidCommitMessage l e) = "Found the following errors\n" : prettyErrors e
  showErrors (ValidCommitMessage _) = ["Everything looks fine"]
  showErrors c = (showErrors . chrisBeamsValidate) c
