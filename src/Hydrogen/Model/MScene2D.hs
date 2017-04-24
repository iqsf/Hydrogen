----------------------------------------------------------------
-- Модуль библиотеки Hydrogen
-- Модель данных (Model)
-- Сцена 2D 
----------------------------------------------------------------

module Hydrogen.Model.MScene2D
    ( Scene2D       (..)
    , SceneHeader2D (..)

    , Color2D

    , Figure2D      (..)
    
    , Texture2D     (..)
    , Model2D       (..)
    
    , scene2D

    , emptyScene2D
    , isEmptyScene2D

    , emptySceneHeader2D
    , isEmptySceneHeader2D

    , isEmptyModel2D

    , texture2D
    , model2D

    , testDebugScene2D
    ) where

-- Импорт модулей
import           Prelude                   as PRL

import           Numeric                      (showHex)

import           GHC.Generics                 (Generic)

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Yaml                 as YAML
import           Data.Char                    (toUpper)
import           Data.Version
import           Data.Typeable
import           Data.Binary

import           Hydrogen.Model.MSceneGeneral

-- data Test3D a = Test3D { testDt :: String
--                        , testPrm :: a
--                        }
--                        deriving (Show, Eq, Typeable, Generic)
-- instance Binary (Test3D String)
-- $(deriveJSON defaultOptions ''Test3D)

-- | Данные 2D сцены
data Scene2D = Scene2D { s2d_name      :: String
                       , s2d_uuid      :: UUID_2D
                       , s2d_version   :: Version
                       , s2d_author    :: String
                       , s2d_descr     :: String
                       , s2d_geoPoint  :: GeoPoint
                       , s2d_tags      :: [String]
                       , s2d_position  :: Position2D
                       , s2d_rotation  :: Rotation2D
                       , s2d_scale     :: Scale2D
                       , s2d_script    :: String   
                       , s2d_envirs    :: [Figure2D]
                       , s2d_children  :: [Figure2D]
                       } 
             | SceneEmpty2D
                       deriving (Show, Eq, Typeable, Generic)


-- | Данные заголовка 2D сцены
data SceneHeader2D = SceneHeader2D { sh2d_name     :: String
                                   , sh2d_uuid     :: UUID_2D
                                   , sh2d_version  :: Version
                                   , sh2d_author   :: String
                                   , sh2d_descr    :: String
                                   , sh2d_geoPoint :: GeoPoint
                                   , sh2d_tags     :: [String]
                                   }
                   | SceneEmptyHeader2D
                                   deriving (Show, Eq, Typeable, Generic)



-- | Цвет 2D
type Color2D = Color3D



-- | Фигуры 2D сцены
data Figure2D = Figure2D
                deriving (Show, Eq, Typeable, Generic)



-- | Данные 2D текстуры
data Texture2D = Texture2D { t2d_name     :: String
                           , t2d_uuid     :: UUID_2D
                           , t2d_version  :: Version
                           , t2d_author   :: String
                           , t2d_descr    :: String
                           , t2d_tags     :: [String]
                           , t2d_contents :: [String]
                           } 
                           deriving (Show, Eq, Typeable, Generic)



-- | Данные 2D модели
data Model2D = Model2D 
               deriving (Show, Eq, Typeable, Generic)



instance Binary Scene2D
instance Binary SceneHeader2D
instance Binary Figure2D
instance Binary Texture2D
instance Binary Model2D 

$(deriveJSON defaultOptions ''Scene2D)
$(deriveJSON defaultOptions ''SceneHeader2D)
$(deriveJSON defaultOptions ''Figure2D)
$(deriveJSON defaultOptions ''Texture2D)
$(deriveJSON defaultOptions ''Model2D)



-- | 2D сцена
scene2D :: Scene2D
scene2D = emptyScene2D


-- | Пустая 2D сцена
emptyScene2D :: Scene2D
emptyScene2D =
    Scene2D { s2d_name      = ""
            , s2d_uuid      = ""
            , s2d_version   = makeVersion [0,0,0]
            , s2d_author    = ""
            , s2d_descr     = ""
            , s2d_geoPoint  = GeoPoint { gp_lat = 0.0, gp_lon = 0.0 }
            , s2d_tags      = []
            , s2d_position  = Position2D { v2d_posX = 0.0, v2d_posY = 0.0 }
            , s2d_rotation  = Rotation2D { v2d_rotZ = 0.0 }
            , s2d_scale     = Scale2D    { v2d_sclX = 1.0, v2d_sclY = 1.0 }
            , s2d_script    = ""
            , s2d_envirs    = []
            , s2d_children  = []
            }



-- | Проверка налиичия пустой сцены
isEmptyScene2D :: Scene2D
               -> Bool
isEmptyScene2D SceneEmpty2D = True
isEmptyScene2D scene2D = 
    let uuid = s2d_uuid scene2D
        name = s2d_name scene2D
    in
    if uuid == "" && name == "" then True else False



-- | Пустой заголовок 2D сцены
emptySceneHeader2D :: SceneHeader2D
emptySceneHeader2D =
    SceneHeader2D { sh2d_name     = ""
                  , sh2d_uuid     = ""
                  , sh2d_version  = makeVersion [0,0,0]
                  , sh2d_author   = ""
                  , sh2d_descr    = ""
                  , sh2d_geoPoint = GeoPoint { gp_lat = 0.0, gp_lon = 0.0 }
                  , sh2d_tags     = []
                  }

-- | Проверка налиичия пустого заголовка сцены
isEmptySceneHeader2D :: SceneHeader2D
                     -> Bool
isEmptySceneHeader2D SceneEmptyHeader2D = True 
isEmptySceneHeader2D sceneHeader2D = 
    let uuid = sh2d_uuid sceneHeader2D
        name = sh2d_name sceneHeader2D
    in
    if uuid == "" && name == "" then True else False



-- | Проверка налиичия пустой 2D модели
isEmptyModel2D :: Model2D
               -> Bool
isEmptyModel2D Model2D = True
--isEmptyModel2D model2D = 
--    let uuid = d2d_uuid model2D
--        name = d2d_name model2D
--    in
--    if uuid == "" && name == "" then True else False



-- | 2D текстура
texture2D :: Texture2D
texture2D = Texture2D { t2d_name     = ""
                      , t2d_uuid     = ""
                      , t2d_version  = makeVersion [0,0,0]
                      , t2d_author   = ""
                      , t2d_descr    = ""
                      , t2d_tags     = []
                      , t2d_contents = []
                      } 



-- | 2D модель
model2D :: Model2D 
model2D = Model2D 



-- | Сцена 2D по умолчанию
defaultScene2D :: Scene2D
defaultScene2D =
    Scene2D { s2d_name      = "Scene 2D (Default)"
            , s2d_uuid      = "3bce9fce-236c-11e7-a910-1b01c59c718e"
            , s2d_version   = makeVersion [0,0,1]
            , s2d_author    = "User"
            , s2d_descr     = "This is description of 2D scene."
            , s2d_geoPoint  = GeoPoint { gp_lat = 0.0, gp_lon = 0.0 }
            , s2d_tags      = [ "Test"
                              , "Demo"
                              , "Debug"
                              ]
            , s2d_position  = Position2D { v2d_posX = 0.0, v2d_posY = 0.0 }
            , s2d_rotation  = Rotation2D { v2d_rotZ = 0.0 }
            , s2d_scale     = Scale2D    { v2d_sclX = 1.0, v2d_sclY = 1.0 }
            , s2d_script    = ""
            , s2d_envirs    = []
            , s2d_children  = []
            }



-- | Разбор конфигурационного файла
parseSceneFile :: String
               -> IO Scene2D
parseSceneFile pathFile = 
    either (error . show) id <$>
    decodeFileEither pathFile





testDebugScene2D :: IO ()
testDebugScene2D = do
    scene <- return $ defaultScene2D
    json  <- return $ toJSON scene
    putStrLn $ show json
    YAML.encodeFile "/home/pavel/Temp/1/Testr.yaml" scene
    --scene <- parseSceneFile "/home/pavel/Projects/Haskell/Adamant/datas/scenes/d7d3e5d4-de50-11e6-80e3-cb4e0b910fb7/MyTest.scene"
    --putStrLn $ show scene
    return ()






