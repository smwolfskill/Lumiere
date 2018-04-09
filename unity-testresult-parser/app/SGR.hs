module SGR
  ( success
  , fail
  , unknown
  , info
  , reset
  , supportsANSI
  ) where

import Prelude hiding       (fail)

import Control.Monad        (when)
import Control.Monad.Reader (MonadReader, asks)
import Control.Monad.Trans  (MonadIO, liftIO)
import System.Console.ANSI  (setSGR, hSupportsANSI, SGR(Reset, SetColor),
                             ConsoleLayer(Foreground), ColorIntensity(..),
                             Color(..))
import System.IO            (stdout)

import Config               (Config, color')

-- does a set of SGR actions in the context of our config
doSGR :: (MonadIO m, MonadReader Config m) => [SGR] -> m ()
doSGR sgr = do
  c <- asks color'
  when c $ liftIO . setSGR $ sgr

-- changes the output color based on the config
success :: (MonadIO m, MonadReader Config m) => m ()
success = doSGR [SetColor Foreground Vivid Green]
fail :: (MonadIO m, MonadReader Config m) => m ()
fail = doSGR [SetColor Foreground Dull Red]
unknown :: (MonadIO m, MonadReader Config m) => m ()
unknown = doSGR [SetColor Foreground Vivid Yellow]
info :: (MonadIO m, MonadReader Config m) => m ()
info = doSGR [SetColor Foreground Dull Yellow]
reset :: (MonadIO m, MonadReader Config m) => m ()
reset = doSGR [Reset]

supportsANSI :: IO Bool
supportsANSI = hSupportsANSI stdout
