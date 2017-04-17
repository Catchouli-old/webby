{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE JavaScriptFFI       #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Game
import Common

import Data.IORef
import Data.Ratio (numerator)
import Data.Time.Clock.POSIX (getPOSIXTime)

import Control.Monad (when)

import Graphics.Rendering.Ombra
import Graphics.Rendering.Ombra.D3
import Graphics.Rendering.Ombra.Draw
import Graphics.Rendering.Ombra.Backend.WebGL

import Control.Monad.IO.Class
import           Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)

import Unsafe.Coerce
import GHCJS.DOM.EventM
import GHCJS.DOM.Element
import GHCJS.DOM.EventTarget
import GHCJS.DOM.EventTargetClosures
import GHCJS.DOM.MouseEvent
import GHCJS.DOM.ClientRect
import GHCJS.DOM.ClientRectList
import GHCJS.DOM.Types
import qualified GHCJS.Types      as T
import qualified Data.JSString    as T
import qualified GHCJS.Foreign    as F
import JavaScript.Web.AnimationFrame (waitForAnimationFrame)


-- | Query a selector

foreign import javascript unsafe
  "document.querySelector($1)"
  query :: T.JSString -> IO T.JSVal


-- | Gets the current time in microseconds

timeMicros :: IO Integer
timeMicros = numerator . toRational . (*1000000) <$> getPOSIXTime


-- Mouse events

mouseEvent :: Element -> ReaderT MouseEvent IO ()
mouseEvent source = do
  x <- ask >>= getOffsetX
  y <- ask >>= getOffsetY
  liftIO . putStrLn $ "x: " ++ show x ++ ", y: " ++ show y


-- Keyboard events

keyboardEvent :: ReaderT KeyboardEvent IO ()
keyboardEvent = liftIO $ putStrLn "key event"


-- | Creates a canvas, context, and enters the main loop

main = do
  canvas <- query "#canvas"
  -- sry about the unsafe coerce. I'm sure there's an idiomatic way to do this
  let canvasElement = unsafeCoerce canvas :: Element
  ctx <- makeContext canvas
  stateRef <- drawState 800 800 >>= newIORef

  -- Initialise game
  drawFunc <- initGame loadTexture

  mouseListener <- newListener (mouseEvent canvasElement)
  keyListener <- newListener keyboardEvent
  addListener canvasElement click mouseListener False
  --addListener canvasElement mouseMove mouseListener False
  addListener canvasElement keyDown keyListener False

  -- Main loop
  let loop lastT fps frames lastFpsUpdate = do
        -- Get current time
        t <- timeMicros

        -- Limit frame rate
        waitForAnimationFrame

        -- Draw function, taking a draw action
        let draw = (flip (refDrawCtx ctx)) stateRef

        -- Draw the game
        draw (drawFunc (fromIntegral t / 2000000.0))

        -- Update frame counter
        let updateFps = (t - lastFpsUpdate) >= 1000000
        let newFps = if updateFps then frames+1 else fps
        let newFrames = if updateFps then 0 else frames+1
        let newLastFpsUpdate = if updateFps then t else lastFpsUpdate

        -- Print the FPS
        when updateFps $ (putStrLn ("updating fps" ++ show newFps))

        -- Loop
        loop t newFps newFrames newLastFpsUpdate

  -- Start main loop
  timeMicros >>= \t -> loop t 0 0 t
