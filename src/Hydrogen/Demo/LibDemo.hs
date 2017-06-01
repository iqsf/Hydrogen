--------------------------------------------------------------
-- Module for demonstrating the work of the library
----------------------------------------------------------------

module Hydrogen.Demo.LibDemo 
    ( testDemo
    ) where

-- Импорт модулей
import           Hydrogen.Hydrogen
import           Hydrogen.Tool.TScene3D                     (parseSceneFile)

import           WebUI.HFitUI
import           WebUI.Scripts.JavaScript.HJavaScript
import           WebUI.Themes.SolarizedUITheme


-- | Demo test
testDemo :: IO ()
testDemo = do
    res <- testOne
    putStrLn res



testOne :: IO String
testOne = do
    scene3D <- parseSceneFile "models/test/MyTest.scene"
--    scene3D <- parseSceneFile "/home/pavel/Projects/Haskell/Hydrogen/models/test/MyTest.scene"
    script  <- return $ runScript defaultHBConfig { hbc_entryLine = "\n" 
                                                  , hbc_tabSpace  = "  "
                                                  , hbc_empty     = ""
                                                  } $ do buildHydroScene DData scene3D "idRoot" "idCanvas" "" 
    return script


data DData = DData

-- | Реализация контекста приложения в котором генерируется сцена
instance CContext3D DData KeyContext3D String where
    contextGet app KeyScenesPath    = "folderScenes"
    contextGet app KeyTexturesPath  = "folderTextures"
    contextGet app KeyModelPath     = "folderModel"
    contextGet app _                = ""

