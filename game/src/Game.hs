module Game (drawGame, scene, test) where

import Graphics.Rendering.Ombra
import Graphics.Rendering.Ombra.Draw
import Graphics.Rendering.Ombra.D3

drawGame :: GLES => Float -> Texture -> Draw ()
drawGame t tex = do drawInit
                    clearBuffers [ColorBuffer, DepthBuffer]
                    drawLayer (scene t tex)

scene :: GLES => Float -> Texture -> Layer
scene t tex = let rotatedCube =   rotY (pi / 4 + t) -- Rotate the cube around the Y
                                              -- axis
                              . rotX (pi / 8)
                              . scale 0.5     -- Scale the cube in every
                                              -- direction
                              $ cube tex      -- The 'cube' function creates
                                              -- a cube with a certain texture.

                  -- Set the view matrix for the group of objects we're going to
                  -- draw.
                  viewGroup = view idmtx         -- In this case, we don't need
                                                 -- to apply any view
                                                 -- transformation, so we use
                                                 -- the identity matrix.

                                   [rotatedCube] -- Our group will only have one
                                                 -- object.

              -- A 'Layer' is a group of objects associated with a program which
              -- contains the GPU instructions on how to render them. 'layerS'
              -- creates a layer associated with a simple 3D rendering program,
              -- which requires a view matrix (otherwise we could have used the
              -- 'mconcat' function instead of 'view' when creating the group).
              in layerS viewGroup


test = putStrLn "game sub project test"
