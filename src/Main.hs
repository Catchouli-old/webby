module Main where

import Render
import JavaScript.Web.AnimationFrame
import GHCJS.Concurrent
import Data.Time.Clock.POSIX
import Data.Ratio
import Control.Concurrent

timeMicros :: IO Integer
timeMicros = numerator . toRational . (*1000000) <$> getPOSIXTime

main = do
  Just rendererInstance <- createRendererInstance
  putStrLn "hi"
  let loop lastT fps frames lastFpsUpdate = do
        t <- timeMicros

        waitForAnimationFrame

        renderGame rendererInstance

        if (t - lastFpsUpdate) >= 1000000
          then putStrLn ("updating fps" ++ show (frames+1)) >> loop t (frames+1) 0 t
          else loop t fps (frames+1) lastFpsUpdate
  timeMicros >>= \t -> loop t 0 0 t
