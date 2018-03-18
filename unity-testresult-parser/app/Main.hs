module Main where

import Prelude hiding      (concat)

import Control.Exception   (try)
import Control.Monad       (sequence_)
import Data.Text           (pack, unpack, concat)
import System.Console.ANSI (setSGR, SGR(Reset, SetColor),
                            ConsoleLayer(Foreground), ColorIntensity(..),
                            Color(..))
import System.Environment  (getArgs, getProgName)
import System.Exit         (exitWith, exitSuccess, ExitCode(ExitFailure))

import Parser              (testResults, TestCase, TestResult(..), testCase,
                            testId, testResult, failureMessage, stackTrace)

-- runs the test result verifier on every input file and exits with failure if
-- at least one test fails
main :: IO ()
main = do
  prog <- getProgName
  args <- getArgs
  if null args then
    putStrLn $ "usage: " ++ prog ++ " <file1> [file2] [...]"
  else do
    results <- sequence $ analyzeFile <$> args
    let fail = or results
    if fail then exitWith (ExitFailure 1) else exitSuccess

-- true if not all test cases succeed
failed :: [TestCase] -> Bool
failed = not . all ((==TestSuccess) . testResult)

-- analyzes a TestResults file given its filename and prints the test results
-- returns whether or not any of the tests failed
analyzeFile :: String -> IO Bool
analyzeFile f = do
  contents' <- try (readFile f) :: IO (Either IOError String)
  case contents' of
       Left _         -> do
         putStrLn $ "could not find file '" ++ f ++ "'"
         return True
       Right contents -> do
         let results = testResults contents
         case results of
              Left e -> do
                putStrLn e
                return True
              Right results' -> do
                let failure = failed results'
                if not failure then
                   setSGR [SetColor Foreground Vivid Green]
                else
                   setSGR [SetColor Foreground Dull Red]
                putStrLn $ f ++ ":"
                setSGR [Reset]
                sequence_ $ outputResult <$> results'
                return failure

-- outputs a single testcase result in pretty colors
outputResult :: TestCase -> IO ()
outputResult testcase = do
  let tc = testCase testcase
      id = testId testcase
      tr = testResult testcase
  putStr . unpack . concat $ [tc, " (", pack (show id), "): "]
  case tr of
       TestSuccess -> do
         setSGR [SetColor Foreground Vivid Green]
         putStrLn "Success"
         setSGR [Reset]
       TestFailure reason -> do
         setSGR [SetColor Foreground Dull Red]
         putStrLn "Failure"
         setSGR [SetColor Foreground Dull Yellow]
         putStrLn "Reason:"
         putStrLn $ unpack $ failureMessage reason
         putStrLn "Stack Trace:"
         putStrLn $ unpack $ stackTrace reason
         setSGR [Reset]
       UnknownResult r -> do
         setSGR [SetColor Foreground Vivid Yellow]
         putStrLn $ unpack r
         setSGR [Reset]
