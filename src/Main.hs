{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE JavaScriptFFI      #-}

module Main where

import Paths_web
import Render
import Game
import JavaScript.Web.AnimationFrame
import qualified GHCJS.DOM as DOM
import qualified GHCJS.DOM.Document as DOM
import qualified GHCJS.DOM.Element as DOM
import qualified GHCJS.DOM.EventTarget as DOM
import qualified GHCJS.DOM.EventTargetClosures as DOM
import GHCJS.Concurrent
import Data.Time.Clock.POSIX
import Data.Ratio
import Control.Concurrent

import qualified GHCJS.Types      as T
import qualified Data.JSString    as T
import qualified GHCJS.Foreign    as F

foreign import javascript unsafe "getStuff()" getStuff :: IO (T.JSString)

timeMicros :: IO Integer
timeMicros = numerator . toRational . (*1000000) <$> getPOSIXTime

main = do
  path <- getDataFileName "test"
  putStrLn . show $ path

  -- Create renderer instance
  Just rendererInstance <- createRendererInstance

  --T.unpack <$> getStuff >>= putStrLn

  -- Create the game instance
  let game = Game (0, 0) [(0.5, 0.5)]

  -- Main loop
  let loop lastT fps frames lastFpsUpdate = do
        -- Get current time
        t <- timeMicros

        -- Limit frame rate
        waitForAnimationFrame

        -- Render the game
        renderGame rendererInstance

        -- Update fps counter if it's been a second, and loop
        if (t - lastFpsUpdate) >= 1000000
          then putStrLn ("updating fps" ++ show (frames+1)) >> loop t (frames+1) 0 t
          else loop t fps (frames+1) lastFpsUpdate

  -- Start main loop
  timeMicros >>= \t -> loop t 0 0 t
