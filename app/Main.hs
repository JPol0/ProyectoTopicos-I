module Main where
import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI
import Control.Monad (void)

main :: IO ()
main = startGUI defaultConfig { jsPort = Just 8023 } setup

setup :: Window -> UI ()
setup w = do
    void $ return w # set UI.title "Demo Threepenny"
    btn <- UI.button #+ [string "Click"]
    out <- UI.span
    void $ getBody w #+ [column [element btn, element out]]
    on UI.click btn $ \_ -> element out # set text "Hola Threepenny!"
