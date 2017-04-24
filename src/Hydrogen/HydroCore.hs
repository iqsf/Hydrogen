----------------------------------------------------------------
-- Модуль библиотеки Hydrogen
-- Инструменты ядра (Core)
-- Ядро генератора
----------------------------------------------------------------

module Hydrogen.HydroCore
    ( CHydrogenScene     (..)
    , CContext3D         (..)
    , cortegeKV
    , CBuilder3D         (..)
    , CBuilderModel3D    (..)
    , CBuilderFragment3D (..)

    , KeyContext3D       (..)
    ) where

-- Импорт модулей
import           Hydrogen.Model.MSceneGeneral

import           WebUI.Scripts.HScript                          (HSL(..), HLFinish(..))
import           WebUI.Scripts.JavaScript.HJSTypes              (HLangJS(..))



-- | Класс сцены библиотеки
-- c   - контекст приложения в котором генерируется сцена
-- k   - ключ обращения
-- v   - значение ответа
-- s   - модель сцены
-- idr - ID корневого элемента
-- idc - ID холста
-- opt - опиции сцены
-- class CContext3D c k v => CHydrogenScene c s idr idc opt where
class CHydrogenScene s idr idc opt where
    buildHydroScene :: (CContext3D c KeyContext3D String) => c -> s -> idr -> idc -> opt -> HSL HLangJS HLFinish



-- | Класс контекста приложения в котором генерируется сцена
-- c - контекст приложения в котором генерируется сцена
-- k - ключ обращения
-- v - значение ответа
class Show k => CContext3D c k v where
    contextGet :: c -> k -> v

    contextIsDebugMode :: (k,v) -> c -> Bool
    contextIsDebugMode _ _ = True



-- | Шалок картежа по умолчанию для класса CContext3D
cortegeKV = ( KeyEmpty :: KeyContext3D
            , ""       :: String
            )



-- | Класс построения 3D сцены по модели данных
-- c - контекст приложения в котором генерируется сцена
-- s - объект сцены
class CBuilder3D s where
    build3D :: (CContext3D c KeyContext3D String) => c -> s -> String -> HSL HLangJS HLangJS



-- | Класс построения 3D модели сцены по модели данных
-- c - контекст приложения в котором генерируется сцена
-- s - объект сцены
class CBuilderModel3D s where
    buildModel3D :: (CContext3D c KeyContext3D String) => c -> s -> String -> (UUID_3D, Position3D, Rotation3D, Scale3D, String) -> HSL HLangJS HLangJS



-- | Класс построения 3D фрагмента сцены по сцене данных
-- c - контекст приложения в котором генерируется сцена
-- s - объект сцены
class CBuilderFragment3D s where
    buildFragment3D :: (CContext3D c KeyContext3D String) => c -> s -> String -> (UUID_3D, Position3D, Rotation3D, Scale3D, String) -> HSL HLangJS HLangJS



-- | Данные ключей контекста
data KeyContext3D = KeyContext3D
				  | KeyScenesPath
                  | KeyTexturesPath 
                  | KeyModelPath 
                  | KeyText String
                  | KeyEmpty

instance Show KeyContext3D where
    show KeyContext3D       = "KeyContext3D"
    show KeyScenesPath      = "scenesPath"
    show KeyTexturesPath    = "texturesPath"
    show KeyModelPath       = "modelPath"
    show (KeyText t)        = t
    show KeyEmpty           = ""



