module App
  ( parseFiles
  ) where

import Prelude hiding       (fail)

import Control.Exception    (try)
import Control.Monad.Reader (MonadReader, asks)
import Control.Monad.Trans  (MonadIO, liftIO)
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
parseFiles :: (MonadIO m, MonadReader Config m) => [String] -> m Bool
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
printSummary :: (MonadIO m, MonadReader Config m) => AggregateResult -> m ()
printSummary (AggregateResult numSuccess numFail numUnknown) = do
  liftIO . putStr $ "\nSummary: "
  success
  liftIO . putStr $ show numSuccess ++ " passed"
  reset
  liftIO . putStr $ ", "
  fail
  liftIO . putStr $ show numFail ++ " failed"
  reset
  liftIO . putStr $ ", "
  unknown
  liftIO . putStr $ show numUnknown ++ " unknown"
  reset
  liftIO . putStr $ "\n"

-- analyzes a TestResults file given its filename and prints the test results
-- returns whether or not any of the tests failed
analyzeFile :: (MonadIO m, MonadReader Config m) => String -> m AggregateResult
analyzeFile f = do
  contents' <- getContents f
  case contents' of
       Left e -> do
         fail
         liftIO . print $ e
         reset
         return $ AggregateResult 0 1 0
       Right contents ->
         case testResults contents of
              Left e -> do
                fail
                liftIO . putStrLn $ e
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
                  liftIO . putStrLn $ f ++ ":"
                  reset
                stuff <- sequence $ outputResult <$> results'
                return $ mconcat stuff
  where getContents :: (MonadIO m, MonadReader Config m)
                    => String
                    -> m (Either IOError String)
        getContents = liftIO . try . readFile
        -- true if not all test cases succeed
        failed :: [TestCase] -> Bool
        failed = not . all ((==TestSuccess) . testResult)


-- outputs a single testcase result in pretty colors
outputResult :: (MonadIO m, MonadReader Config m)
             => TestCase
             -> m AggregateResult
outputResult testcase = do
  let tc = testCase testcase
      id = testId testcase
      tr = testResult testcase
      stuff = [tc, " (", pack (show id), "): "]
      prefix = liftIO . putStr . unpack . mconcat $ stuff
  quiet <- asks quiet'
  case tr of
       TestSuccess -> do
         if quiet then
           pure ()
         else do
           prefix
           success
           liftIO . putStrLn $ "Success"
           reset
         return $ AggregateResult 1 0 0
       TestFailure reason -> do
         prefix
         fail
         liftIO . putStrLn $ "Failure"
         info
         -- lift an entire IO block
         liftIO $ do
           putStrLn "Reason:"
           putStrLn . unpack . failureMessage $ reason
           putStrLn "Stack Trace:"
           putStrLn . unpack . stackTrace $ reason
         reset
         return $ AggregateResult 0 1 0
       UnknownResult r -> do
         prefix
         unknown
         liftIO . putStrLn . unpack $ r
         reset
         return $ AggregateResult 0 0 1
