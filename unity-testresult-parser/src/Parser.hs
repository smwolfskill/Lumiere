module Parser
  ( testResults
  , TestCase
  , TestResult(..)
  , FailureReason
  , testCase
  , testId
  , testResult
  , failureMessage
  , stackTrace
  ) where

import Prelude hiding      (lookup)

import Data.Map            (lookup)
import Data.Maybe          (fromMaybe)
import Data.Text           (pack, unpack, Text, append, strip)
import Text.XML            (parseText, def, documentRoot, Element(Element),
                            nameLocalName, Node(NodeContent, NodeElement),
                            Name(Name))

import qualified Data.Text.Lazy as L

-- type to hold why a specific test failed
data FailureReason = FailureReason
                   { failureMessage :: Text
                   , stackTrace :: Text
                   }
                   deriving (Show, Eq)

-- type to hold a general test result
data TestResult = TestSuccess
                | TestFailure FailureReason
                | UnknownResult Text
                deriving (Show, Eq)

-- type to hold data for a general test case
data TestCase = TestCase
              { testCase :: Text
              , testId :: Int
              , testResult :: TestResult
              }
              deriving (Show, Eq)

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
       "test-case" -> return $ TestCase name (read (unpack id)) testresult
          where name = fromMaybe "" (a !? "name")
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
