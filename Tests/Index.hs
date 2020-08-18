module Tests.Index where

type SuccessMessage = String

data Test = Test {name :: String, success :: Bool}

data TestExecution = Ok SuccessMessage | Err [Test]

instance Show TestExecution where
  show (Ok message) = message
  show (Err tests) = concat $ failHeading : map (breakLine . name) tests
    where
      failHeading = "The following tests failed:\n"
      breakLine = (++ "\n")

runTest :: [Test] -> TestExecution
runTest x = if null failed then Ok message else Err failed
  where
    failed = filter (not . success) x
    message = "All tests succeeded."
