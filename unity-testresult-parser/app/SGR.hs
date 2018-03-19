module SGR
  ( success
  , fail
  , unknown
  , info
  , reset
  , supportsANSI
  ) where

import Prelude hiding       (fail)

import Control.Monad.Reader (ReaderT, asks)
import Control.Monad.Trans  (lift)
import System.Console.ANSI  (setSGR, hSupportsANSI, SGR(Reset, SetColor),
                             ConsoleLayer(Foreground), ColorIntensity(..),
                             Color(..))
import System.IO            (stdout)

import Config               (Config, color')

-- does a set of SGR actions in the context of our config
doSGR :: [SGR] -> ReaderT Config IO ()
doSGR sgr = do
  c <- asks color'
  if c then
    lift . setSGR $ sgr
  else
    pure ()

-- changes the output color based on the config
success :: ReaderT Config IO ()
success = doSGR [SetColor Foreground Vivid Green]
fail :: ReaderT Config IO ()
fail = doSGR [SetColor Foreground Dull Red]
unknown :: ReaderT Config IO ()
unknown = doSGR [SetColor Foreground Vivid Yellow]
info :: ReaderT Config IO ()
info = doSGR [SetColor Foreground Dull Yellow]
reset :: ReaderT Config IO ()
reset = doSGR [Reset]

supportsANSI :: IO Bool
supportsANSI = hSupportsANSI stdout
