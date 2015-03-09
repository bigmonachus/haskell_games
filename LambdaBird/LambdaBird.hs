module Main where

import Control.Monad
import qualified Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL as GL
import Reactive.Banana as R
import Reactive.Banana.Frameworks as R
import System.IO
import System.Exit

errorCallback :: GLFW.ErrorCallback
errorCallback err desc = hPutStrLn stderr desc

keyCallback :: GLFW.KeyCallback
keyCallback w key scancode action mods = undefined

resize w h = do print "resize"

type Vec2 = (GLfloat, GLfloat)


drawTriangles :: [Vec2] -> IO()
drawTriangles triList = do
    GL.color $ Color3 0 0 (0 :: GLfloat)
    GL.renderPrimitive Triangles $ mapM_ (\(x, y) -> vertex (Vertex2 x y)) triList

mainLoop win = do
    shouldClose <- GLFW.windowShouldClose win
    unless shouldClose $ do
        GL.clearColor $= Color4 1 1 1 1
        GL.clear [ColorBuffer]
        drawTriangles [(-1,0), (1,0), (0,1)]
        GLFW.swapBuffers win
        GLFW.pollEvents
        mainLoop win

main = let width   = 1024
           height  = 768
           fps     = 60
           hz      = 1 / 60 :: Float
        in do
            GLFW.init
            GLFW.defaultWindowHints
            Just win <- GLFW.createWindow width height "Lambda Bird" Nothing Nothing
            GLFW.makeContextCurrent (Just win)
            GLFW.setWindowSizeCallback win $ Just (\_ w h -> resize w h)
            GLFW.setKeyCallback win $ (Just keyCallback)
            GLFW.swapInterval 1
            mainLoop win
            GLFW.destroyWindow win
            GLFW.terminate
            exitSuccess
