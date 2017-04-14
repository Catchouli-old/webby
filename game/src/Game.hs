module Game (initGame, drawGame) where

import Graphics.Rendering.Ombra
import Graphics.Rendering.Ombra.Draw
import Graphics.Rendering.Ombra.D3
import Graphics.Rendering.Ombra.Vector

initGame :: GLES => (String -> IO Texture) -> IO (Float -> Draw ())
initGame loadTexture = do
  tex <- loadTexture "data/images/tex.png"
  return $ drawGame tex

drawGame :: GLES => Texture -> Float -> Draw ()
drawGame tex t = do drawInit
                    clearBuffers [ColorBuffer, DepthBuffer]
                    drawLayer (scene t tex)

scene :: GLES => Float -> Texture -> Layer
scene t tex = let rotatedCube =   rotY (pi / 4 + t)
                              . rotX (pi / 8)
                              . scale 0.5
                              $ cube tex
                  viewGroup = view idmtx [rotatedCube]
              in layerS viewGroup

