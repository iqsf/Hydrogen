----------------------------------------------------------------
-- Модуль библиотеки Hydrogen
-- Модель данных (Model)
-- Сцена 3D 
----------------------------------------------------------------

module Hydrogen.Model.MScene3D
    ( Scene3D       (..)
    , SceneHeader3D (..)
    , Material      (..)

    , Camera3D      (..)
    , Figure3D      (..)
    , Texture3D     (..)
    , Model3D       (..)
    
    , ECOptions3D   (..)
    , ECScreen3D    (..)

    , scene3D

    , emptyScene3D
    , isEmptyScene3D

    , emptySceneHeader3D
    , isEmptySceneHeader3D

    , isEmptyModel3D

    , texture3D
    , model3D

    , testDebugScene3D
    ) where

-- Импорт модулей
import           Prelude                   as PRL

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

-- | Данные 3D сцены
data Scene3D = Scene3D { s3d_name      :: String
                       , s3d_uuid      :: UUID_3D
                       , s3d_version   :: Version
                       , s3d_author    :: String
                       , s3d_descr     :: String
                       , s3d_geoPoint  :: GeoPoint
                       , s3d_tags      :: [String]
                       , s3d_position  :: Position3D
                       , s3d_rotation  :: Rotation3D
                       , s3d_scale     :: Scale3D
                       , s3d_script    :: String   
                       , s3d_materials :: [Material]
                       , s3d_camera    :: Camera3D
                       , s3d_envirs    :: [Figure3D]
                       , s3d_children  :: [Figure3D]
                       }
             | SceneEmpty3D
                       deriving (Show, Eq, Typeable, Generic)



-- | Данные заголовка 3D сцены
data SceneHeader3D = SceneHeader3D { sh3d_name     :: String
                                   , sh3d_uuid     :: UUID_3D
                                   , sh3d_version  :: Version
                                   , sh3d_author   :: String
                                   , sh3d_descr    :: String
                                   , sh3d_geoPoint :: GeoPoint
                                   , sh3d_tags     :: [String]
                                   }
                   | SceneEmptyHeader3D
                                   deriving (Show, Eq, Typeable, Generic)



-- | Материал 3D сцены
data Material = Material        { m3d_name              :: String
                                , m3d_uuid              :: UUID_3D
                                } 
              | MaterialLambert { m3d_name              :: String
                                , m3d_uuid              :: UUID_3D 
                                , m3d_color             :: Color3D
                                , m3d_emissive          :: Color3D
                                , m3d_emissiveIntensity :: Double
                                , m3d_opacity           :: Double
                                , m3d_transparent       :: Bool                               
                                , m3d_script            :: String   
                                } 
              | MaterialBasic   { m3d_name              :: String
                                , m3d_uuid              :: UUID_3D 
                                , m3d_texture           :: UUID_3D
                                , m3d_opacity           :: Double
                                , m3d_transparent       :: Bool                               
                                , m3d_script            :: String   
                                } 
              | MaterialFace    { m3d_name              :: String
                                , m3d_uuid              :: UUID_3D 
                                , m3d_texture           :: UUID_3D
                                , m3d_opacity           :: Double
                                , m3d_transparent       :: Bool                               
                                , m3d_script            :: String   
                                } 
                                deriving (Show, Eq, Typeable, Generic)



-- | Камера 3D
data Camera3D = Camera3D
              | CameraPerspective3D  { c3d_name     :: String
                                     , c3d_uuid     :: UUID_3D
                                     , c3d_position :: Position3D
                                     , c3d_scale    :: Scale3D
                                     , c3d_fov      :: Double
                                     , c3d_near     :: Double
                                     , c3d_far      :: Double
                                     , c3d_zoom     :: Double
                                     , c3d_up       :: Vector3D
                                     , c3d_script   :: String   
                                     , c3d_children :: [Figure3D]
                                     }
              | CameraOrthographic3D { c3d_name     :: String
                                     , c3d_uuid     :: UUID_3D
                                     , c3d_position :: Position3D
                                     , c3d_scale    :: Scale3D
                                     , c3d_near     :: Double
                                     , c3d_far      :: Double
                                     , c3d_zoom     :: Double
                                     , c3d_up       :: Vector3D
                                     , c3d_script   :: String   
                                     , c3d_children :: [Figure3D]
                                     }
                                     deriving (Show, Eq, Typeable, Generic)



-- | Фигуры 3D сцены
data Figure3D = Figure3D
              | FGroup3D    { f3d_name     :: String
                            , f3d_uuid     :: UUID_3D
                            , f3d_flagDraw :: Bool
                            , f3d_position :: Position3D
                            , f3d_rotation :: Rotation3D
                            , f3d_scale    :: Scale3D
                            , f3d_script   :: String   
                            , f3d_children :: [Figure3D]
                            }
              | FVGroup3D   { f3d_name     :: String
                            , f3d_uuid     :: UUID_3D
                            , f3d_flagDraw :: Bool
                            , f3d_children :: [Figure3D]
                            }
              | FModel3D    { f3d_name     :: String
                            , f3d_uuid     :: UUID_3D
                            , f3d_flagDraw :: Bool
                            , f3d_material :: UUID_3D
                            , f3d_position :: Position3D
                            , f3d_rotation :: Rotation3D
                            , f3d_scale    :: Scale3D
                            , f3d_model    :: UUID_3D
                            , f3d_script   :: String   
                            }
              | FFragment3D { f3d_name     :: String
                            , f3d_uuid     :: UUID_3D
                            , f3d_flagDraw :: Bool
                            , f3d_position :: Position3D
                            , f3d_rotation :: Rotation3D
                            , f3d_scale    :: Scale3D
                            , f3d_scene    :: UUID_3D
                            , f3d_script   :: String   
                            }
              | FLAmbientLight3D     { l3d_name        :: String
                                     , l3d_uuid        :: UUID_3D
                                     , l3d_flagDraw    :: Bool
                                     , l3d_color       :: String
                                     , l3d_intensity   :: Double
                                     , l3d_script      :: String   
                                     }
              | FLPointLight3D       { l3d_name        :: String
                                     , l3d_uuid        :: UUID_3D
                                     , l3d_flagDraw    :: Bool
                                     , l3d_color       :: String
                                     , l3d_intensity   :: Double
                                     , l3d_distance    :: Double
                                     , l3d_decay       :: Double
                                     , l3d_position    :: Position3D
                                     , l3d_script      :: String   
                                     }
              | FLDirectionalLight3D { l3d_name        :: String
                                     , l3d_uuid        :: UUID_3D
                                     , l3d_flagDraw    :: Bool
                                     , l3d_color       :: String
                                     , l3d_intensity   :: Double
                                     , l3d_position    :: Position3D
                                     , l3d_script      :: String   
                                     }
              | FLHemisphereLight3D  { l3d_name        :: String
                                     , l3d_uuid        :: UUID_3D
                                     , l3d_flagDraw    :: Bool
                                     , l3d_colorSky    :: String
                                     , l3d_colorGround :: String
                                     , l3d_intensity   :: Double
                                     , l3d_position    :: Position3D
                                     , l3d_script      :: String   
                                     }
              | FLSpotLight3D        { l3d_name        :: String
                                     , l3d_uuid        :: UUID_3D
                                     , l3d_flagDraw    :: Bool
                                     , l3d_color       :: String
                                     , l3d_intensity   :: Double
                                     , l3d_distance    :: Double
                                     , l3d_angle       :: Double
                                     , l3d_penumbra    :: Double
                                     , l3d_decay       :: Double
                                     , l3d_position    :: Position3D
                                     , l3d_script      :: String   
                                     }
              | FBox3D    { f3d_name     :: String
                          , f3d_uuid     :: UUID_3D
                          , f3d_flagDraw :: Bool
                          , f3d_material :: UUID_3D
                          , f3d_position :: Position3D
                          , f3d_rotation :: Rotation3D
                          , f3d_scale    :: Scale3D
                          , f3d_sizeX    :: Double
                          , f3d_sizeY    :: Double
                          , f3d_sizeZ    :: Double
                          , f3d_script   :: String   
                          } 
              | FPlane3D  { f3d_name     :: String
                          , f3d_uuid     :: UUID_3D
                          , f3d_flagDraw :: Bool
                          , f3d_material :: UUID_3D
                          , f3d_position :: Position3D
                          , f3d_rotation :: Rotation3D
                          , f3d_scale    :: Scale3D
                          , f3d_sizeX    :: Double
                          , f3d_sizeY    :: Double
                          , f3d_script   :: String   
                          }
              | FScript3D { f3d_name     :: String
                          , f3d_uuid     :: UUID_3D
                          , f3d_flagDraw :: Bool
                          , f3d_material :: UUID_3D
                          , f3d_position :: Position3D
                          , f3d_rotation :: Rotation3D
                          , f3d_scale    :: Scale3D
                          , f3d_flagExt  :: Bool
                          , f3d_params   :: [String]
                          , f3d_scriptDo :: String   
                          , f3d_script   :: String   
                          }
              | FEntityCamera3D { e3d_name     :: String
                                , e3d_uuid     :: UUID_3D
                                , e3d_flagDraw :: Bool
                                , e3d_position :: Position3D
                                , e3d_rotation :: Rotation3D
                                , e3d_scale    :: Scale3D
                                , e3d_params   :: String
                                , e3d_source   :: String
                                , e3d_options  :: ECOptions3D
                                , e3d_opacity  :: Double
                                , e3d_scrnSide :: String
                                , e3d_screens  :: [ECScreen3D]
                                , e3d_script   :: String   
                                }
              deriving (Show, Eq, Typeable, Generic)



-- | Данные 3D текстуры
data Texture3D = Texture3D { t3d_name     :: String
                           , t3d_uuid     :: UUID_3D
                           , t3d_version  :: Version
                           , t3d_author   :: String
                           , t3d_descr    :: String
                           , t3d_tags     :: [String]
                           , t3d_contents :: [String]
                           } 
                           deriving (Show, Eq, Typeable, Generic)



-- | Данные 3D модели
data Model3D = Model3D 
             | Model3D_Obj  { d3d_name     :: String
                            , d3d_uuid     :: UUID_3D
                            , d3d_version  :: Version
                            , d3d_author   :: String
                            , d3d_descr    :: String
                            , d3d_tags     :: [String]
                            , d3d_cntObjs  :: [String]
                            , d3d_cntMtls  :: [String]
                            } 
             | Model3D_Json { d3d_name     :: String
                            , d3d_uuid     :: UUID_3D
                            , d3d_version  :: Version
                            , d3d_author   :: String
                            , d3d_descr    :: String
                            , d3d_tags     :: [String]
                            , d3d_contents :: [String]
                            } 
                           deriving (Show, Eq, Typeable, Generic)



-- Настройки отображения камеры сущности FEntityCamera3D
data ECOptions3D = ECOptions3D Bool         -- ^ Флаг отображения
                               String       -- ^ Цвет камеры
                               Double       -- ^ Пропорции 
                               Double       -- ^ Масштаб отображения
                               deriving (Show, Eq, Typeable, Generic)

-- Экран для сущности FEntityCamera3D
data ECScreen3D = ECScreen3D Int     -- ^ Number

                             Double  -- ^ Sx
                             Double  -- ^ Sy

                             Double  -- ^ px
                             Double  -- ^ py
                             Double  -- ^ pz

                             Double  -- ^ rx
                             Double  -- ^ ry
                             Double  -- ^ rz

                             Double  -- ^ sx
                             Double  -- ^ sy
                             Double  -- ^ sz

                             Double  -- ^ dx0
                             Double  -- ^ dy0
                             Double  -- ^ dz0

                             Double  -- ^ dx1
                             Double  -- ^ dy1
                             Double  -- ^ dz1

                             Double  -- ^ dx2
                             Double  -- ^ dy2
                             Double  -- ^ dz2

                             Double  -- ^ dx3
                             Double  -- ^ dy3
                             Double  -- ^ dz3
                             
                             Double Double  -- ^ (ax0,ay0)
                             Double Double  -- ^ (ax1,ay1)
                             Double Double  -- ^ (ax2,ay2)
                             Double Double  -- ^ (ax3,ay3)
                             deriving (Show, Eq, Typeable, Generic)



instance Binary Scene3D
instance Binary SceneHeader3D
instance Binary Material
instance Binary Camera3D
instance Binary Figure3D
instance Binary Texture3D
instance Binary Model3D 
instance Binary ECOptions3D 
instance Binary ECScreen3D 

$(deriveJSON defaultOptions ''Scene3D)
$(deriveJSON defaultOptions ''SceneHeader3D)
$(deriveJSON defaultOptions ''Material)
$(deriveJSON defaultOptions ''Camera3D)
$(deriveJSON defaultOptions ''Figure3D)
$(deriveJSON defaultOptions ''Texture3D)
$(deriveJSON defaultOptions ''Model3D)
$(deriveJSON defaultOptions ''ECOptions3D)
$(deriveJSON defaultOptions ''ECScreen3D)



-- | 3D сцена
scene3D :: Scene3D
scene3D = emptyScene3D


-- | Пустая 3D сцена
emptyScene3D :: Scene3D
emptyScene3D =
    Scene3D { s3d_name      = ""
            , s3d_uuid      = ""
            , s3d_version   = makeVersion [0,0,0]
            , s3d_author    = ""
            , s3d_descr     = ""
            , s3d_geoPoint  = GeoPoint { gp_lat = 0.0, gp_lon = 0.0 }
            , s3d_tags      = []
            , s3d_position  = Position3D { v3d_posX = 0.0, v3d_posY = 0.0, v3d_posZ = 0.0 }
            , s3d_rotation  = Rotation3D { v3d_rotX = 0.0, v3d_rotY = 0.0, v3d_rotZ = 0.0 }
            , s3d_scale     = Scale3D    { v3d_sclX = 1.0, v3d_sclY = 1.0, v3d_sclZ = 1.0 }
            , s3d_script    = ""
            , s3d_materials = []
            , s3d_camera    = Camera3D  
            , s3d_envirs    = []
            , s3d_children  = []
            }

-- | Проверка налиичия пустой сцены
isEmptyScene3D :: Scene3D
               -> Bool
isEmptyScene3D SceneEmpty3D = True
isEmptyScene3D scene3D = 
    let uuid = s3d_uuid scene3D
        name = s3d_name scene3D
    in
    if uuid == "" && name == "" then True else False



-- | Пустой заголовок 3D сцены
emptySceneHeader3D :: SceneHeader3D
emptySceneHeader3D =
    SceneHeader3D { sh3d_name     = ""
                  , sh3d_uuid     = ""
                  , sh3d_version  = makeVersion [0,0,0]
                  , sh3d_author   = ""
                  , sh3d_descr    = ""
                  , sh3d_geoPoint = GeoPoint { gp_lat = 0.0, gp_lon = 0.0 }
                  , sh3d_tags     = []
                  }

-- | Проверка налиичия пустого заголовка сцены
isEmptySceneHeader3D :: SceneHeader3D
                     -> Bool
isEmptySceneHeader3D SceneEmptyHeader3D = True
isEmptySceneHeader3D sceneHeader3D = 
    let uuid = sh3d_uuid sceneHeader3D
        name = sh3d_name sceneHeader3D
    in
    if uuid == "" && name == "" then True else False



-- | Проверка налиичия пустой 3D модели
isEmptyModel3D :: Model3D
               -> Bool
isEmptyModel3D Model3D = True
isEmptyModel3D model3D = 
    let uuid = d3d_uuid model3D
        name = d3d_name model3D
    in
    if uuid == "" && name == "" then True else False



-- | 3D текстура
texture3D :: Texture3D
texture3D = Texture3D { t3d_name     = ""
                      , t3d_uuid     = ""
                      , t3d_version  = makeVersion [0,0,0]
                      , t3d_author   = ""
                      , t3d_descr    = ""
                      , t3d_tags     = []
                      , t3d_contents = []
                      } 



-- | 3D модель
model3D :: Model3D 
model3D = Model3D 



-- | Сцена 3D по умолчанию
defaultScene3D :: Scene3D
defaultScene3D =
    Scene3D { s3d_name      = "Scene 3D (Default)"
            , s3d_uuid      = "d7d3e5d4-de50-11e6-80e3-cb4e0b910fb7"
            , s3d_version   = makeVersion [0,0,1]
            , s3d_author    = "User"
            , s3d_descr     = "This is description of 3D scene."
            , s3d_geoPoint  = GeoPoint { gp_lat = 0.0, gp_lon = 0.0 }
            , s3d_tags      = [ "Test"
                              , "Demo"
                              , "Debug"
                              ]
            , s3d_position  = Position3D { v3d_posX = 0.0, v3d_posY = 0.0, v3d_posZ = 0.0 }
            , s3d_rotation  = Rotation3D { v3d_rotX = 0.0, v3d_rotY = 0.0, v3d_rotZ = 0.0 }
            , s3d_scale     = Scale3D    { v3d_sclX = 1.0, v3d_sclY = 1.0, v3d_sclZ = 1.0 }
            , s3d_script    = ""
            , s3d_materials = [ Material        { m3d_name              = "Default material"
                                                , m3d_uuid              = "0ea4f586-e2fb-11e6-8150-938210091cb8"
                                                } 
                              , MaterialLambert { m3d_name              = "MyMaterial 1"
                                                , m3d_uuid              = "239e3faa-de51-11e6-b31a-3b6189350ce9"
                                                , m3d_color             = ColorRGB_3D 110 110 110
                                                , m3d_emissive          = ColorRGB_3D 110 110 110
                                                , m3d_emissiveIntensity = 1.0
                                                , m3d_opacity           = 1.0
                                                , m3d_transparent       = False
                                                , m3d_script            = ""
                                                }
                              , MaterialLambert { m3d_name              = "MyMaterial 2"
                                                , m3d_uuid              = "4466976e-de51-11e6-a5d3-f3eee64202b6"
                                                , m3d_color             = ColorRGB_3D 10 145 10
                                                , m3d_emissive          = ColorRGB_3D 110 110 110
                                                , m3d_emissiveIntensity = 1.0
                                                , m3d_opacity           = 1.0
                                                , m3d_transparent       = False
                                                , m3d_script            = ""
                                                }
                              ]
            , s3d_camera    = CameraPerspective3D { c3d_name     = "MyCamera" 
                                                  , c3d_uuid     = "e501c034-ef7e-11e6-ab73-87e2eb608649"
                                                  , c3d_position = Position3D { v3d_posX = 0.0, v3d_posY = -10.0, v3d_posZ = 10.0 }
                                                  , c3d_scale    = Scale3D    { v3d_sclX = 1.0, v3d_sclY = 1.0, v3d_sclZ = 1.0 }
                                                  , c3d_fov      = 45
                                                  , c3d_near     = 1
                                                  , c3d_far      = 10
                                                  , c3d_zoom     = 1.0
                                                  , c3d_up       = Vector3D {v3d_x = 0.0, v3d_y = 0.0, v3d_z = 1.0 }
                                                  , c3d_script   = ""   
                                                  , c3d_children = []
                                                  }
            , s3d_envirs    = []
            , s3d_children  = [ FGroup3D { f3d_name = "MyGroup"
                                         , f3d_uuid = "20c55aec-e15e-11e6-9b09-a74605bb9055"
                                         , f3d_flagDraw = True
                                         , f3d_position = Position3D { v3d_posX = 0.0, v3d_posY = 0.0, v3d_posZ = 0.0 }
                                         , f3d_rotation = Rotation3D { v3d_rotX = 0.0, v3d_rotY = 0.0, v3d_rotZ = 0.0 }
                                         , f3d_scale    = Scale3D    { v3d_sclX = 0.0, v3d_sclY = 0.0, v3d_sclZ = 0.0 }
                                         , f3d_script   = ""   
                                         , f3d_children = [ FBox3D   { f3d_name  = "MyBox"
                                                                     , f3d_uuid  = "1a49493a-de52-11e6-8860-4725afee8c70"
                                                                     , f3d_flagDraw = True
                                                                     , f3d_material = "239e3faa-de51-11e6-b31a-3b6189350ce9"
                                                                     , f3d_position = Position3D { v3d_posX = 0.0, v3d_posY = 0.0, v3d_posZ = 0.0 }
                                                                     , f3d_rotation = Rotation3D { v3d_rotX = 0.0, v3d_rotY = 0.0, v3d_rotZ = 0.0 }
                                                                     , f3d_scale    = Scale3D    { v3d_sclX = 1.0, v3d_sclY = 1.0, v3d_sclZ = 1.0 }
                                                                     , f3d_sizeX = 3
                                                                     , f3d_sizeY = 2
                                                                     , f3d_sizeZ = 1
                                                                     , f3d_script   = ""   
                                                                     } 
                                                          , FPlane3D { f3d_name  = "MyPlane"
                                                                     , f3d_uuid  = "22f6f794-de52-11e6-b97a-9bf7977de4ff"
                                                                     , f3d_flagDraw = True
                                                                     , f3d_material = "4466976e-de51-11e6-a5d3-f3eee64202b6"
                                                                     , f3d_position = Position3D { v3d_posX = 0.0, v3d_posY = 0.0, v3d_posZ = 0.0 }
                                                                     , f3d_rotation = Rotation3D { v3d_rotX = 0.0, v3d_rotY = 0.0, v3d_rotZ = 0.0 }
                                                                     , f3d_scale    = Scale3D    { v3d_sclX = 1.0, v3d_sclY = 1.0, v3d_sclZ = 1.0 }
                                                                     , f3d_sizeX = 5
                                                                     , f3d_sizeY = 5
                                                                     , f3d_script   = ""   
                                                                     }
                                                          , FEntityCamera3D { e3d_name     = "DeomoCamera3D"
                                                                            , e3d_uuid     = "56dfc5fc-edef-11e6-8474-e3fd097df198"
                                                                            , e3d_flagDraw = True
                                                                            , e3d_position = Position3D { v3d_posX = 0.0, v3d_posY = 0.0, v3d_posZ = 0.0 }
                                                                            , e3d_rotation = Rotation3D { v3d_rotX = 0.0, v3d_rotY = 0.0, v3d_rotZ = 0.0 }
                                                                            , e3d_scale    = Scale3D    { v3d_sclX = 1.0, v3d_sclY = 1.0, v3d_sclZ = 1.0 }
                                                                            , e3d_params   = ""
                                                                            , e3d_source   = "/video/sintel.ogv"
                                                                            , e3d_options  = ECOptions3D True "0xFF0000" 0.5 1.0
                                                                            , e3d_opacity  = 1.0
                                                                            , e3d_scrnSide = ""
                                                                            , e3d_screens  = [ ECScreen3D 0   2.0 2.0   0.0 0.0 3.0   0.0 0.0 0.0   1.0 1.0 1.0    0.0 0.0 0.0    0.0 0.0 0.0   0.0 0.0 0.0    0.0 0.0 0.0  0.0 1.0  0.5 1.0  0.5 0.0  0.0 0.0
                                                                                             , ECScreen3D 1   2.0 1.0   0.0 3.0 4.0   0.0 0.0 0.0   1.0 1.0 1.0    0.0 0.0 0.0    0.0 0.0 0.0   0.0 0.0 0.0    0.0 0.0 0.0  0.5 1.0  1.0 1.0  1.0 0.0  0.5 0.0
                                                                                             ]
                                                                            , e3d_script   = ""
                                                                            }
                                                          ]
                                         }
                              ]
            }



-- | Разбор конфигурационного файла
parseSceneFile :: String
               -> IO Scene3D
parseSceneFile pathFile = 
    either (error . show) id <$>
    decodeFileEither pathFile





testDebugScene3D :: IO ()
testDebugScene3D = do
    scene <- return $ defaultScene3D
    json  <- return $ toJSON scene
    putStrLn $ show json
    YAML.encodeFile "/home/pavel/Temp/1/Testr.yaml" scene
    --scene <- parseSceneFile "/home/pavel/Projects/Haskell/Adamant/datas/scenes/d7d3e5d4-de50-11e6-80e3-cb4e0b910fb7/MyTest.scene"
    --putStrLn $ show scene
    return ()




