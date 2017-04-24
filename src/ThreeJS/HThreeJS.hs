----------------------------------------------------------------
-- Модуль приложения
-- Скрипты ThreeJS для WebGL (HThreeJS)
-- Фреймворк ThreeJS 
----------------------------------------------------------------

module ThreeJS.HThreeJS
    ( detectorWebGL
    ) where

-- Импорт модулей
import           Prelude                                     as PRL

import           Data.Char                                      (toLower)
import           Data.String.Utils                              (strip)

import           WebUI.Scripts.HScript
import           WebUI.Scripts.JavaScript.HJavaScript



-- | Детектор WebGL
detectorWebGL :: HSL HLangJS HLangJS 
detectorWebGL = do  
    c <- ask
    hl <- return $ HL $ "if(!Detector.webgl) Detector.addGetWebGLMessage();" ++ (hbc_entryLine c)
    modify (:> hl)
    return hl



