module Main where

import Control.Monad.Reader (runReaderT)
import Options.Applicative  (execParser)
import System.Exit          (exitWith, exitSuccess, ExitCode(ExitFailure))

import App                  (parseFiles)
import Config               (Config(Config))
import Options              (optionParser, color, quiet, summary, files)
import SGR                  (supportsANSI)

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
            "auto" -> supportsANSI
            _      -> pure False

  let cfg = Config c (quiet opts) (summary opts)
      args = files opts

  result <- runReaderT (parseFiles args) cfg
  if result then exitWith (ExitFailure 1) else exitSuccess
