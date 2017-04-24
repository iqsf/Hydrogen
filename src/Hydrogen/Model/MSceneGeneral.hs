----------------------------------------------------------------
-- Модуль библиотеки Hydrogen
-- Модель данных (Model)
-- Общин данные для сцены 
----------------------------------------------------------------

module Hydrogen.Model.MSceneGeneral
    ( GeoPoint      (..)

    , Color3D       (..)

    , Position2D    (..)
    , Position3D    (..)

    , Rotation2D    (..)
    , Rotation3D    (..)

    , Scale2D       (..)
    , Scale3D       (..)

    , Vector2D      (..)
    , Vector3D      (..)

    , UUID_2D       (..)
    , UUID_3D       (..)
    ) where



-- Импорт модулей
import           Prelude                   as PRL

import           Numeric                      (showHex)

import           GHC.Generics                 (Generic)

--import           Data.Aeson
import           Data.Aeson.TH
--import           Data.Yaml                 as YAML
import           Data.Char                    (toUpper)
--import           Data.Version
import           Data.Typeable
import           Data.Binary



-- | Данне географической точки
data GeoPoint = GeoPoint { gp_lat :: Double
                         , gp_lon :: Double
                         }
                         deriving (Show, Eq, Typeable, Generic)



-- | Цвет 3D
data Color3D = Color3D
             | ColorNull3D 
             | ColorRGB_3D  Int Int Int
             | ColorRGBA_3D Int Int Int Int
             | ColorRGB_H_3D  String String String
             | ColorRGBA_H_3D String String String String
             | ColorH_3D String
             deriving (Eq, Typeable, Generic)

instance Show Color3D where
    show Color3D                  = "0xFFFFFF"
    show ColorNull3D              = "null"
    show (ColorRGB_3D    r g b  ) = "0x" ++ (showHex r "") ++ (showHex g "") ++ (showHex b "")
    show (ColorRGBA_3D   r g b a) = "0x" ++ (showHex r "") ++ (showHex g "") ++ (showHex b "") ++ (showHex a "")
    show (ColorRGB_H_3D  r g b  ) = "0x" ++ (map toUpper r) ++ (map toUpper g) ++ (map toUpper b)
    show (ColorRGBA_H_3D r g b a) = "0x" ++ (map toUpper r) ++ (map toUpper g) ++ (map toUpper b) ++ (map toUpper a)
    show (ColorH_3D      v      ) = "0x" ++ (map toUpper v)



-- | Линейное положение в 2D пространстве
data Position2D = Position2D { v2d_posX :: Double
                             , v2d_posY :: Double
                             }
                             deriving (Show, Eq, Typeable, Generic)

-- | Линейное положение в 3D пространстве
data Position3D = Position3D { v3d_posX :: Double
                             , v3d_posY :: Double
                             , v3d_posZ :: Double
                             }
                             deriving (Show, Eq, Typeable, Generic)



-- | Угловое положение в 2D пространстве
data Rotation2D = Rotation2D { v2d_rotZ :: Double
                             }
                             deriving (Show, Eq, Typeable, Generic)

-- | Угловое положение в 3D пространстве
data Rotation3D = Rotation3D { v3d_rotX :: Double
                             , v3d_rotY :: Double
                             , v3d_rotZ :: Double
                             }
                             deriving (Show, Eq, Typeable, Generic)



-- | Масштаб в 2D пространстве
data Scale2D = Scale2D { v2d_sclX :: Double
                       , v2d_sclY :: Double
                       }
                       deriving (Show, Eq, Typeable, Generic)

-- | Масштаб в 3D пространстве
data Scale3D = Scale3D { v3d_sclX :: Double
                       , v3d_sclY :: Double
                       , v3d_sclZ :: Double
                       }
                       deriving (Show, Eq, Typeable, Generic)



-- | Вектор в 2D пространстве
data Vector2D = Vector2D { v2d_x :: Double
                         , v2d_y :: Double
                         }
                         deriving (Show, Eq, Typeable, Generic)

-- | Вектор в 3D пространстве
data Vector3D = Vector3D { v3d_x :: Double
                         , v3d_y :: Double
                         , v3d_z :: Double
                         }
                         deriving (Show, Eq, Typeable, Generic)



-- | Синоним типа для UUID
type UUID_2D = String 
type UUID_3D = String 



instance Binary GeoPoint
instance Binary Color3D
instance Binary Position2D
instance Binary Position3D
instance Binary Rotation2D
instance Binary Rotation3D
instance Binary Scale2D
instance Binary Scale3D
instance Binary Vector2D
instance Binary Vector3D



$(deriveJSON defaultOptions ''GeoPoint)
$(deriveJSON defaultOptions ''Color3D)
$(deriveJSON defaultOptions ''Position2D)
$(deriveJSON defaultOptions ''Position3D)
$(deriveJSON defaultOptions ''Rotation2D)
$(deriveJSON defaultOptions ''Rotation3D)
$(deriveJSON defaultOptions ''Scale2D)
$(deriveJSON defaultOptions ''Scale3D)
$(deriveJSON defaultOptions ''Vector2D)
$(deriveJSON defaultOptions ''Vector3D)



