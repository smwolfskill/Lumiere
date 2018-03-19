module Main where

import Prelude hiding       (concat, fail)

import Control.Exception    (try)
import Control.Monad        (sequence_)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Trans  (lift)
import Data.Text            (pack, unpack, concat)
import Options.Applicative  (execParser)
import System.Exit          (exitWith, exitSuccess, ExitCode(ExitFailure))
import System.IO            (stdout)

import Config               (Config(..))
import Options              (optionParser, color, quiet, summary, files)
import Parser               (testResults, TestCase, TestResult(..), testCase,
                             testId, testResult, failureMessage, stackTrace)
import SGR                  (hSupportsANSI, success, fail, unknown, info, reset)

-- runs the test result verifier on every input file and exits with failure if
-- at least one test fails
main :: IO ()
main = do
  -- parse all the options
  opts <- execParser optionParser

  -- configure color output
  c <- case color opts of
            "yes"  -> pure True
            "no"   -> pure False
            "auto" -> hSupportsANSI stdout
            _      -> pure False

  let cfg = Config c (quiet opts) (summary opts)
      args = files opts

  -- get IO actions and failures for each file, and then 'sequence' them one
  -- after the other in IO
  results <- sequence $ flip runReaderT cfg . analyzeFile <$> args
  if or results then exitWith (ExitFailure 1) else exitSuccess

-- analyzes a TestResults file given its filename and prints the test results
-- returns whether or not any of the tests failed
analyzeFile :: String -> ReaderT Config IO Bool
analyzeFile f = do
  contents' <- getContents f
  case contents' of
       Left e -> do
         fail
         lift . print $ e
         reset
         return True
       Right contents ->
         case testResults contents of
              Left e -> do
                fail
                lift . putStrLn $ e
                reset
                return True
              Right results' -> do
                let failure = failed results'
                if not failure then success else fail
                lift . putStrLn $ f ++ ":"
                reset
                sequence_ $ outputResult <$> results'
                return failure
  where getContents :: String -> ReaderT Config IO (Either IOError String)
        getContents = lift . try . readFile
        -- true if not all test cases succeed
        failed :: [TestCase] -> Bool
        failed = not . all ((==TestSuccess) . testResult)


-- outputs a single testcase result in pretty colors
outputResult :: TestCase -> ReaderT Config IO ()
outputResult testcase = do
  let tc = testCase testcase
      id = testId testcase
      tr = testResult testcase
  lift . putStr . unpack . concat $ [tc, " (", pack (show id), "): "]
  case tr of
       TestSuccess -> do
         success
         lift . putStrLn $ "Success"
         reset
       TestFailure reason -> do
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
       UnknownResult r -> do
         unknown
         lift . putStrLn . unpack $ r
         reset
