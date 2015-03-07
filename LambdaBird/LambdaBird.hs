module Main where

import Control.Concurrent.STM (TQueue, atomically, newTQueueIO, tryReadTQueue, writeTQueue)
import Control.Monad (unless, when, void)
import Graphics.UI.GLFW as GLFW

data Event =
    EventError              !GLFW.Error    !String
  | EventWindowPos          !GLFW.Window   !Int !Int
  | EventWindowSize         !GLFW.Window   !Int !Int
  | EventWindowClose        !GLFW.Window
  | EventWindowRefresh      !GLFW.Window
  | EventWindowFocus        !GLFW.Window   !GLFW.FocusState
  | EventWindowIconify      !GLFW.Window   !GLFW.IconifyState
  | EventFramebufferSize    !GLFW.Window   !Int !Int
  | EventMouseButton        !GLFW.Window   !GLFW.MouseButton !GLFW.MouseButtonState !GLFW.ModifierKeys
  | EventCursorPos          !GLFW.Window   !Double !Double
  | EventCursorEnter        !GLFW.Window   !GLFW.CursorState
  | EventScroll             !GLFW.Window   !Double !Double
  | EventKey                !GLFW.Window   !GLFW.Key !Int !GLFW.KeyState !GLFW.ModifierKeys
  | EventChar               !GLFW.Window   !Char
  deriving Show

main = do
        eventsChan <- newTQueueIO :: IO (TQueue Event)
        withWindow width height "LambdaBird" $ \win -> do
            putStrLn "end!"
        where width = 1024
              height = 768

withWindow :: Int -> Int -> String -> (GLFW.Window -> IO()) -> IO ()
withWindow width height title f =
    do
        GLFW.setErrorCallback $ Just simpleErrorCallback
        r <- GLFW.init
        when r $
            do
                m <- GLFW.createWindow width height title Nothing Nothing
                case m of
                    (Just win) -> do
                        GLFW.makeContextCurrent m
                        f win
                        GLFW.setErrorCallback $ Just simpleErrorCallback
                        GLFW.destroyWindow win
                    Nothing -> return ()
                GLFW.terminate

simpleErrorCallback e s = putStrLn $ unwords [show e, show s]

