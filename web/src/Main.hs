{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE JavaScriptFFI      #-}

module Main where

import Graphics.Rendering.Ombra
import Graphics.Rendering.Ombra.Backend.WebGL
import Graphics.Rendering.Ombra.D3
import Common

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
import Game

import Graphics.Rendering.Ombra.Backend.WebGL

import Graphics.Rendering.Ombra.Draw
import Data.IORef

import qualified GHCJS.Types      as T
import qualified Data.JSString    as T
import qualified GHCJS.Foreign    as F


-- | Query a selector

foreign import javascript unsafe
  "document.querySelector($1)"
  query :: T.JSString -> IO T.JSVal


-- | Gets the current time in microseconds

timeMicros :: IO Integer
timeMicros = numerator . toRational . (*1000000) <$> getPOSIXTime


-- | Creates a canvas, context, and enters the main loop

main = do
  canvas <- query "#canvas"
  ctx <- makeContext canvas
  stateRef <- drawState 800 800 >>= newIORef

  -- Initialise game
  drawFunc <- initGame loadTexture

  -- Main loop
  let loop lastT fps frames lastFpsUpdate = do
        -- Get current time
        t <- timeMicros

        -- Limit frame rate
        waitForAnimationFrame

        -- Draw function, taking a draw action
        let draw = (flip (refDrawCtx ctx)) stateRef

        draw (drawFunc (fromIntegral t / 2000000.0))

        -- Update fps counter if it's been a second, and loop
        if (t - lastFpsUpdate) >= 1000000
          then putStrLn ("updating fps" ++ show (frames+1)) >> loop t (frames+1) 0 t
          else loop t fps (frames+1) lastFpsUpdate

  -- Start main loop
  timeMicros >>= \t -> loop t 0 0 t
