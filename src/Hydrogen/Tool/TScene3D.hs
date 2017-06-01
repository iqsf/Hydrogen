----------------------------------------------------------------
-- Модуль приложения
-- Инструменты для модели данных (Tool)
-- Инструментарий сцены 3D 
----------------------------------------------------------------

module Hydrogen.Tool.TScene3D
    ( parseSceneFile
    , loadScene3D
    , loadTexture3D
    , loadModel3D
    , sayDebugMessage
    ) where

-- Импорт модулей
import           Prelude                   as PRL

import           System.Directory

import           Data.String.Utils
import           Data.Yaml                 as YAML

import           Hydrogen.HydroCore
import           Hydrogen.Model.MSceneGeneral
import           Hydrogen.Model.MScene3D



-----------------------------------------------------------------------------------------------
-- Работа с моделью 3D   ----------------------------------------------------------------------
-----------------------------------------------------------------------------------------------

-- | Разбор файла 3D сцены
parseSceneFile :: String
               -> IO Scene3D
parseSceneFile pathFile = 
    either (error . show) id <$>
    decodeFileEither pathFile



-- | Разбор файла 3D текстуры
parseTextureFile :: String
                 -> IO Texture3D
parseTextureFile pathFile = 
    either (error . show) id <$>
    decodeFileEither pathFile



-- | Разбор файла 3D текстуры
parseModelFile :: String
               -> IO Model3D
parseModelFile pathFile = 
    either (error . show) id <$>
    decodeFileEither pathFile



-- | Поиск файла в списке имен по фильтру
findFileFilter :: [String] 
               -> String 
               -> String
findFileFilter []     _    = ""
findFileFilter (x:xs) txtF = if (endswith txtF x) == True
                             then x
                             else findFileFilter xs txtF



-- | Загрузить 3D сцену
loadScene3D :: (CContext3D a KeyContext3D String)
            => a            -- ^ Основной тип данных приложения
            -> UUID_3D      -- ^ UUID 3D сцены
            -> IO Scene3D   -- ^ 3D сцена
loadScene3D app uuid3D = do
    pathDir  <- return $ (contextGet app KeyScenesPath) ++ "/" ++ uuid3D
    contents <- getDirectoryContents pathDir
    file3D   <- return $ pathDir ++ "/" ++ (findFileFilter contents ".scene")
    flagIs3D <- doesFileExist file3D
    sayDebugMessage app $ "FILE SCENE: " ++ file3D
    if flagIs3D == True
    then parseSceneFile file3D
    else return emptyScene3D



-- | Загрузить 3D текстуру
loadTexture3D :: (CContext3D a KeyContext3D String)
              => a              -- ^ Основной тип данных приложения
              -> UUID_3D        -- ^ UUID 3D текстуры
              -> IO Texture3D   -- ^ 3D текстура
loadTexture3D app uuid3D = do
    pathDir       <- return $ (contextGet app KeyTexturesPath) ++ "/" ++ uuid3D
    contents      <- getDirectoryContents pathDir
    fileTexture3D <- return $ pathDir ++ "/" ++ (findFileFilter contents ".tx")
    sayDebugMessage app $ "FILE TEXTURE: " ++ fileTexture3D
    texture3D  <- parseTextureFile fileTexture3D
    return texture3D



-- | Загрузить 3D модель
loadModel3D :: (CContext3D a KeyContext3D String)
            => a              -- ^ Основной тип данных приложения
            -> UUID_3D        -- ^ UUID 3D модели
            -> IO Model3D     -- ^ 3D модель
loadModel3D app uuid3D = do
    pathDir     <- return $ (contextGet app KeyModelPath) ++ "/" ++ uuid3D
    contents    <- getDirectoryContents pathDir
    fileModel3D <- return $ pathDir ++ "/" ++ (findFileFilter contents ".d3d")
    sayDebugMessage app $ "FILE MODEL: " ++ fileModel3D
    model3D <- parseModelFile fileModel3D
    return model3D



-- | Вывести отладочное сообщение
sayDebugMessage :: (CContext3D a KeyContext3D String)
                => a            -- ^ Основной тип данных приложения
                -> String       -- ^ Отладочное сообщение
                -> IO ()        -- ^ Результат
sayDebugMessage app message = do
    if (contextIsDebugMode cortegeKV app) == True 
    then putStrLn message 
    else return ()



