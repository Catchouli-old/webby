{-# LANGUAGE OverloadedStrings #-}

module Main where

import Game
import Graphics.Rendering.Ombra
import Graphics.Rendering.Ombra.Draw
import Graphics.Rendering.Ombra.Backend.OpenGL
import Data.IORef
import Codec.Picture
import Codec.Picture.Types (promoteImage)
import Control.Monad (unless)
import qualified Data.Vector.Storable as V
import qualified SDL as SDL
import qualified SDL.Time as SDL


-- | The fixed timestep for updates

timestep = 1.0 / 60.0


-- | The title of the window

title = ""


-- | The fixed width and height of the window

width, height :: Integer
(width, height) = (800, 800)


-- | The entry point for the native (windowed) application

main :: IO ()
main = do
  -- Initialize sdl
  SDL.initializeAll

  -- Window parameters
  let windowDims = SDL.V2 (fromIntegral width) (fromIntegral height)
  let windowDesc = SDL.defaultWindow { SDL.windowOpenGL = Just SDL.defaultOpenGL
                                     , SDL.windowInitialSize = windowDims
                                     }
  
  ticks <- SDL.ticks

  -- Create window and context
  window <- SDL.createWindow title windowDesc
  context <- SDL.glCreateContext window

  -- Create ombra context
  stateRef <- drawState 800 800 >>= newIORef
  ctx <- makeContext

  -- Initialise game
  drawFunc <- initGame loadTexture

  let draw = (flip (refDrawCtx ctx)) stateRef

  -- Main loop
  let loop t = do events <- SDL.pollEvents
                  let eventPayloads = map SDL.eventPayload events
                  let quitting = elem SDL.QuitEvent eventPayloads
                  ticks <- SDL.ticks
                  let elapsed = fromIntegral (ticks) / 5000.0
                  draw (drawFunc elapsed)
                  SDL.glSwapWindow window
                  unless quitting (loop ticks)

  SDL.ticks >>= loop

  -- Cleanup
  SDL.glDeleteContext context
  SDL.destroyWindow window
  SDL.quit


-- | Load a texture
-- | I stole this. That is why the formatting is awful.

loadTexture :: FilePath -> IO Texture
loadTexture path = do eimg <- readImage path
                      case eimg of
                           Left err -> error err 
                           Right img ->
                                   case convertRGBA8 img of
                                        Image w h v -> return . mkTexture w h False $ 
                                                        [colList v]
        where colList = fst . V.foldr (\x (l, cs) ->
                                        case cs of
                                             [g, b, a] -> ( Color x g b a : l 
                                                          , [] )
                                             _ -> (l, x : cs) 
                                      )   
                                      ([], []) 

