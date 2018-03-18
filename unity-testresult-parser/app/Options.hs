module Options
  ( Options(Options)
  , color
  , quiet
  , summary
  , files
  , optionParser
  ) where

import Data.List.Split     (splitOn)
import Data.Monoid         ((<>), mempty)
import Data.Text           (pack, unpack, strip)
import Options.Applicative ((<**>), Parser, ParserInfo, strOption, switch, long,
                            short, value, metavar, help, info, helper, fullDesc,
                            progDesc, header)

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
          <> help "quiets output (only show failed) (not implemented)"
           )
       <*> switch
           ( long "summary"
          <> short 's'
          <> help "print test result summary (total passed/failed) (not implemented)"
           )
       <*> ( fmap (unpack . strip . pack) . splitOn "," <$>
             strOption
             ( long "files"
            <> short 'f'
            <> metavar "FILES=file1[,file2,...]"
            <> help "files to parse (commas not allowed in path)"
             )
           )

-- construct the actual parser
optionParser :: ParserInfo Options
optionParser = info withHelp infomod
  where withHelp = options <**> helper
        infomod = fullDesc
               <> progDesc "reads every file in FILES and prints out the test results with failure messages and detailed stack traces"
               <> header "unity-testresult-parser -- a parser for unity test results"
