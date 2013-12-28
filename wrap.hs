import Displays
import Graphics.UI.GLUT
--In all use cases so far, it turns out that I do not need a reshape callback
--for my windowing system to reshape the drawing window.
--($=) :: HasSetter s => s a -> a -> IO ()
main :: IO ()
main = do
    (_progName, _args) <- getArgsAndInitialize
    _window <- createWindow "Rotating Functions Around A Circle"
    displayCallback $= Displays.lineSpirals
    mainLoop
