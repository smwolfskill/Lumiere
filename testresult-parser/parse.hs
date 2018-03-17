{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding      (lookup, concat)

import Control.Exception   (try)
import Control.Monad       (sequence_)
import Data.Map            (lookup)
import Data.Maybe          (fromMaybe)
import Data.Text           (pack, unpack, Text, append, concat, strip)
import System.Console.ANSI (setSGR, SGR(Reset, SetColor),
                            ConsoleLayer(Foreground), ColorIntensity(..),
                            Color(..))
import System.Environment  (getArgs, getProgName)
import System.Exit         (exitWith, exitSuccess, ExitCode(ExitFailure))
import Text.XML            (parseText, def, documentRoot, Element(Element),
                            nameLocalName, Node(NodeContent, NodeElement),
                            Name(Name))

import qualified Data.Text.Lazy as L

-- type to hold why a specific test failed
data FailureReason = FailureReason
                   { failureMessage :: Text,
                     stackTrace :: Text
                   }
                   deriving (Show, Eq)

-- type to hold a general test result
data TestResult = TestSuccess
                | TestFailure FailureReason
                | UnknownResult Text
                deriving (Show, Eq)

-- type to hold data for a general test case
data TestCase = TestCase
              { testCase :: Text,
                testResult :: TestResult
              }
              deriving (Show, Eq)

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

  where failed :: [TestCase] -> Bool
        failed [] = False
        failed (TestCase _ tr : tc') = case tr of
                                            TestSuccess -> failed tc'
                                            _           -> True

-- outputs a single testcase result in pretty colors
outputResult :: TestCase -> IO ()
outputResult (TestCase tc tr) = do
  putStr $ unpack tc ++ ": "
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

-- parses the contents of a TestResults file
testResults :: String -> Either String [TestCase]
testResults results =
  case parseText def (L.pack results) of
       Left _    -> Left "could not parse"
       Right doc -> Right $ accumulateResults (documentRoot doc)

-- given an XML root element, find all <test-case> elements and try to extract
-- success/failure
accumulateResults :: Element -> [TestCase]
accumulateResults (Element name a nodes) =
  case nameLocalName name of
       "test-case" -> return $ TestCase testcase testresult
          where testcase = concat [name, " (", id, ")"]
                name = fromMaybe "" (a !? "name")
                id = fromMaybe "?" (a !? "id")
                testresult = case fromMaybe "result not found" (a !? "result") of
                                  "Passed" -> TestSuccess
                                  "Failed" -> TestFailure reason
                                  res      -> UnknownResult res
                  where failure :: Text
                        failure = strip $ fromMaybe "<no message found>" failure'
                        failures = findFailure <$> children
                        failure' = case filter (/= Nothing) failures of
                                        (m:_) -> m
                                        _     -> Nothing
                        reason = FailureReason failure "<not implemented yet>"
       _           -> concatMap accumulateResults children
 where children = elements nodes

-- attempts to find a failure message for a test that failed
findFailure :: Element -> Maybe Text
findFailure (Element name a nodes) =
  case nameLocalName name of
       "failure" -> case filter (/= Nothing) (findMessage <$> children) of
                         (m:_) -> m
                         _     -> Nothing
       _         -> case filter (/= Nothing) (findFailure <$> children) of
                         (m:_) -> m
                         _     -> Nothing
  where children = elements nodes

-- attempts to retrieve the message data
findMessage :: Element -> Maybe Text
findMessage (Element name a nodes) =
  case nameLocalName name of
       "message" -> fromContent (head nodes)
       _         -> case filter (/= Nothing) (findMessage <$> children) of
                         (m:_) -> m
                         _     -> Nothing
  where children = elements nodes

-- gets all the xml elements from a list of xml nodes
elements :: [Node] -> [Element]
elements [] = []
elements (NodeElement e:es) = e : elements es
elements (_:es) = elements es

-- extracts content from a node
fromContent :: Node -> Maybe Text
fromContent (NodeContent t) = Just t
fromContent _ = Nothing

-- shorthand for hashmap lookup that returns a Maybe type
(!?) = flip lookup
