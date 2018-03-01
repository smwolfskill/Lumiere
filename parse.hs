{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding     (lookup, concat)

import Control.Arrow      ((***))
import Control.Exception  (try)
import Control.Monad      (sequence_)
import Data.Map           (lookup)
import Data.Maybe         (fromMaybe)
import Data.Text          (pack, unpack, Text, append, concat)
import System.Environment (getArgs, getProgName)
import System.Exit        (exitWith, exitSuccess, ExitCode(ExitFailure))
import Text.XML           (parseText, def, documentRoot, Element(Element),
                           nameLocalName, Node(NodeContent, NodeElement),
                           Name(Name))

import qualified Data.Text.Lazy as L

main :: IO ()
main = do
  prog <- getProgName
  args <- getArgs
  if null args then
    putStrLn $ "usage: " ++ prog ++ " <file1> [file2] [...]"
  else do
    results <- sequence $ analyzeFile <$> args
    let (output, fail) = foldl (\(r, f) -> (r++) *** (f||)) ([], False) results
    sequence_ . fmap putStrLn $ output
    if fail then exitWith (ExitFailure 1) else exitSuccess

analyzeFile :: String -> IO ([String], Bool)
analyzeFile f = do
  contents' <- try (readFile f) :: IO (Either IOError String)
  case contents' of
       Left _         -> return (["could not find file '" ++ f ++ "'"], True)
       Right contents -> do
         let results = testResults contents
         return ((f ++ ":") : results, failed results)

  where failed :: [String] -> Bool
        failed = any (isSubstring "Failed: ")
        isSubstring :: String -> String -> Bool
        isSubstring a b =
          case length a `compare` length b of
               LT -> a == take (length a) b || isSubstring a (tail b)
               EQ -> a == b
               GT -> False

testResults :: String -> [String]
testResults results =
  case parseText def (L.pack results) of
       Left _    -> ["error: could not parse"]
       Right doc -> accumulateResults (documentRoot doc)

accumulateResults :: Element -> [String]
accumulateResults (Element name a nodes) =
  case nameLocalName name of
       "test-case" -> [testcase ++ ": " ++ testresult]
          where testcase = unpack $ concat [name, " (", id, ")"]
                name = fromMaybe "" (a !? "name")
                id = fromMaybe "?" (a !? "id")
                testresult = unpack result
                result = case fromMaybe "result not found" (a !? "result") of
                              "Failed" -> "Failed: " `append` failure
                              res      -> res
                  where failure :: Text
                        failure = fromMaybe "<no message found>" failure'
                        failures = findFailure <$> children
                        failure' = case filter (/= Nothing) failures of
                                        (m:_) -> m
                                        _     -> Nothing
       _           -> concatMap accumulateResults children
 where children = elements nodes

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

findMessage :: Element -> Maybe Text
findMessage (Element name a nodes) =
  case nameLocalName name of
       "message" -> fromContent (head nodes)
       _         -> case filter (/= Nothing) (findMessage <$> children) of
                         (m:_) -> m
                         _     -> Nothing
  where children = elements nodes

elements :: [Node] -> [Element]
elements [] = []
elements (NodeElement e:es) = e : elements es
elements (_:es) = elements es

fromContent :: Node -> Maybe Text
fromContent (NodeContent t) = Just t
fromContent _ = Nothing

(!?) = flip lookup
