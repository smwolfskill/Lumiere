module Options
  ( Options(Options)
  , color
  , quiet
  , summary
  , files
  , optionParser
  ) where

import Control.Applicative
  (many)
import Data.List.Split
  (splitOn)
import Data.Monoid
  ((<>), mempty)
import Data.Text
  (pack, unpack, strip)
import Options.Applicative
  (Parser, ParserInfo, argument, str, strOption, switch, long, short, value,
   metavar, help, helpDoc, info, helper, fullDesc, progDesc, header, footerDoc)
import Text.PrettyPrint.ANSI.Leijen
  (text, linebreak)

data Options = Options
             { color :: String
             , quiet :: Bool
             , summary :: Bool
             , files :: [String]
             }
  deriving (Show, Eq)

-- optparse option parser
options :: Parser Options
options = Options
       <$> strOption
           ( long "color"
          <> short 'c'
          <> value "auto"
          <> metavar "yes|no|auto"
          <> help "colorizes output"
           )
       <*> switch
           ( long "quiet"
          <> short 'q'
          <> help "only show non-passing results"
           )
       <*> switch
           ( long "summary"
          <> short 's'
          <> help "print test result summary (total passed/failed)"
           )
       <*> many
           ( argument str
             ( metavar "FILES"
            <> helpDoc
               ( Just $ text "FILES=[file1] [file2] ..."
              <> linebreak
              <> text "files to parse"
               )
             )
           )

-- construct the actual parser
optionParser :: ParserInfo Options
optionParser = info withHelp infomod
  where withHelp = helper <*> options
        infomod = fullDesc
               <> progDesc "reads every file in FILES and prints out the test results with failure messages and detailed stack traces"
               <> header "unity-testresult-parser -- a parser for unity test results"
               <> footerDoc
                  ( Just $ text "example usage:"
                 <> linebreak
                 <> text ("unity-testresult-parser --color=auto --quiet " <>
                          "--summary TestResults.xml")
                  )
