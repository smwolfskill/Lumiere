module App
  ( parseFiles
  ) where

import Prelude hiding       (fail)

import Control.Exception    (try)
import Control.Monad.Reader (ReaderT, asks)
import Control.Monad.Trans  (lift)
import Data.Text            (pack, unpack)

import Config               (Config(..))
import Parser               (testResults, TestCase, TestResult(..), testCase,
                             testId, testResult, failureMessage, stackTrace)
import SGR                  (success, fail, unknown, info, reset)

-- monoidable type which will aggregate our test result count
data AggregateResult = AggregateResult Int Int Int
  deriving (Show, Eq)

instance Monoid AggregateResult where
  AggregateResult a b c `mappend` AggregateResult a' b' c' = newresult
    where newresult = AggregateResult (a + a') (b + b') (c + c')
  mempty = AggregateResult 0 0 0

-- parses a list of files and prints out a total summary
parseFiles :: [String] -> ReaderT Config IO Bool
parseFiles files = do
  -- analyzefile <$> args :: [ReaderT Config IO AggregateResult]
  -- sequence the list to get an ReaderT Config IO [AggregateResult]
  results <- sequence $ analyzeFile <$> files
  let results'@(AggregateResult _ nf _) = mconcat results
  summary <- asks summary'
  if summary then
    printSummary results'
  else
    pure ()
  return (nf /= 0)

-- prints a summary of an aggregate of results
printSummary :: AggregateResult -> ReaderT Config IO ()
printSummary (AggregateResult numSuccess numFail numUnknown) = do
  lift . putStr $ "\nSummary: "
  success
  lift . putStr $ show numSuccess ++ " passed"
  reset
  lift . putStr $ ", "
  fail
  lift . putStr $ show numFail ++ " failed"
  reset
  lift . putStr $ ", "
  unknown
  lift . putStr $ show numUnknown ++ " unknown"
  reset
  lift . putStr $ "\n"

-- analyzes a TestResults file given its filename and prints the test results
-- returns whether or not any of the tests failed
analyzeFile :: String -> ReaderT Config IO AggregateResult
analyzeFile f = do
  contents' <- getContents f
  case contents' of
       Left e -> do
         fail
         lift . print $ e
         reset
         return $ AggregateResult 0 1 0
       Right contents ->
         case testResults contents of
              Left e -> do
                fail
                lift . putStrLn $ e
                reset
                return $ AggregateResult 0 1 0
              Right results' -> do
                let failure = failed results'
                quiet <- asks quiet'
                if quiet && not failure then
                  pure ()
                  -- we can return AggregateResult (length results') 0 0 to
                  -- avoid calling outputResult a bunch of times but
                  -- outputResult handles quiet as well, so this should work as
                  -- well
                else do
                  if not failure then success else fail
                  lift . putStrLn $ f ++ ":"
                  reset
                stuff <- sequence $ outputResult <$> results'
                return $ mconcat stuff
  where getContents :: String -> ReaderT Config IO (Either IOError String)
        getContents = lift . try . readFile
        -- true if not all test cases succeed
        failed :: [TestCase] -> Bool
        failed = not . all ((==TestSuccess) . testResult)


-- outputs a single testcase result in pretty colors
outputResult :: TestCase -> ReaderT Config IO AggregateResult
outputResult testcase = do
  let tc = testCase testcase
      id = testId testcase
      tr = testResult testcase
      stuff = [tc, " (", pack (show id), "): "]
      prefix = lift . putStr . unpack . mconcat $ stuff
  quiet <- asks quiet'
  case tr of
       TestSuccess -> do
         if quiet then
           pure ()
         else do
           prefix
           success
           lift . putStrLn $ "Success"
           reset
         return $ AggregateResult 1 0 0
       TestFailure reason -> do
         prefix
         fail
         lift . putStrLn $ "Failure"
         info
         -- lift an entire IO block
         lift $ do
           putStrLn "Reason:"
           putStrLn . unpack . failureMessage $ reason
           putStrLn "Stack Trace:"
           putStrLn . unpack . stackTrace $ reason
         reset
         return $ AggregateResult 0 1 0
       UnknownResult r -> do
         prefix
         unknown
         lift . putStrLn . unpack $ r
         reset
         return $ AggregateResult 0 0 1
