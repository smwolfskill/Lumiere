module Config
  ( Config(..)
  ) where

data Config = Config
            { color' :: Bool
            , quiet' :: Bool
            , summary' :: Bool
            }
  deriving (Eq, Show)
