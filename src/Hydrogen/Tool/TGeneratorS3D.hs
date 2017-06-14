----------------------------------------------------------------
-- Модуль библиотеки Hydrogen
-- Инструменты для модели данных (Tool)
-- Генератор WebGL кода сцены 3D 
----------------------------------------------------------------

module Hydrogen.Tool.TGeneratorS3D
    ( 
    ) where

-- Импорт модулей
import           Prelude                                     as PRL

import           Data.Char                                      (toLower)
import           Data.String.Utils                              (replace)

import           Text.Julius

import           WebUI.HFitUI
import           WebUI.Scripts.JavaScript.HJavaScript
import           WebUI.Themes.SolarizedUITheme

import           ThreeJS.HThreeJS

import           WebUI.HFitUI
import           WebUI.Scripts.JavaScript.HJavaScript

import           Hydrogen.HydroCore
import           Hydrogen.Model.MSceneGeneral
import           Hydrogen.Model.MScene3D

import           Hydrogen.Tool.TScene3D
import           Hydrogen.Tool.TBuilder3D



instance CHydrogenScene Scene3D String String String where
    buildHydroScene app scene3D idRoot idCanvas sopts = generateWGLScene3D app scene3D idRoot idCanvas sopts



-- | Сгенерировать WGL код сцены 3D
generateWGLScene3D :: (CContext3D c KeyContext3D String) 
                   => c                     -- ^ Контекст приложения в котором генерируется сцена
                   -> Scene3D               -- ^ Модель 3D сцены
                   -> String                -- ^ ID корневого элемента
                   -> String                -- ^ ID холста
                   -> String                -- ^ Опиции 3D сцены
                   -> HSL HLangJS HLFinish  -- ^ Результат
--generateWGLScene3D _   SceneEmpty _ _ _ = do
--    return endH
--generateWGLScene3D _   (Scene2D {}) _ _ _ = do
--    return endH
generateWGLScene3D app scene3D idRoot idCanvas sopts = do
    (//) "Автоматически сгенерированный скрипт"
    (//) "Приложение Hydrogen (0.0.1)"

    detectorWebGL >> hjsBR 

    var_camera    <- newVar "camera"    HJsEmpty
    var_scene     <- newVar "scene"     HJsEmpty
    var_groupRoot <- newVar "groupRoot" HJsEmpty
    var_root      <- newVar "root"      $ getElementById idRoot
    var_canvas3D  <- newVar "canvas3D"  $ getElementById idCanvas 
    var_renderer  <- newVar "renderer"  HJsEmpty
    var_stats     <- newVar "stats"     HJsEmpty

    -- | Управление в 3D сцене
    hjs $[julius|
        var orbitControls = null;
        var flyControls = null;
    |]

    hjsBR

    -- Рендер сцены
    varFn_render <- functJS "render" [] $ do 
        hjs $[julius| renderer.render(scene, camera); |]
        return endH

    hjsBR

    -- Обновление
    varFn_update <- functJS "update" [] $ do 
        hjs $[julius| if(stats) stats.update(); |]
        hjs $[julius| if(flyControls) flyControls.update(1); |]
        return endH

    hjsBR

    -- Анимация
    varFn_animate <- functJS "animate" [] $ do 
        hjs $[julius| requestAnimationFrame(animate); |]
        call varFn_render []
        call varFn_update []
        return endH

    hjsBR

    -- Отработка изменения размера окна
    varFn_onWindowResize <- functJS "onWindowResize" [] $ do 
        hjs $[julius|    
            if (camera) {
                camera.aspect = window.innerWidth / (window.innerHeight);
                camera.updateProjectionMatrix();
            }
            renderer.setSize( window.innerWidth, window.innerHeight);
        |]
        call varFn_render []
        return endH

    hjsBR
    
    -- Отрисовка корневых осей сцены
    varFn_drawRootAxes <- functJS "drawRootAxes" [] $ do 
        hjs $[julius|
            var dirX = new THREE.Vector3( 1, 0, 0 ); dirX.normalize();
            var dirY = new THREE.Vector3( 0, 1, 0 ); dirY.normalize();
            var dirZ = new THREE.Vector3( 0, 0, 1 ); dirZ.normalize();

            var origin = new THREE.Vector3( 0, 0, 0 );
            var length = 1;

            var hexX = 0xff0000;
            var hexY = 0x00ff00;
            var hexZ = 0x0000ff;

            scene.add(new THREE.ArrowHelper(dirX, origin, length, hexX));
            scene.add(new THREE.ArrowHelper(dirY, origin, length, hexY));
            scene.add(new THREE.ArrowHelper(dirZ, origin, length, hexZ));
        |]
        return endH

    hjsBR

    -- Инициализация
    varFn_init <- functJS "init" [] $ do 

        hjs $[julius|
            scene = new THREE.Scene();

            groupRoot = new THREE.Group();
            groupRoot.position.set( #{rawJS (show $ v3d_posX $ s3d_position scene3D)} , #{rawJS (show $ v3d_posY $ s3d_position scene3D)} , #{rawJS (show $ v3d_posZ $ s3d_position scene3D)} );
            groupRoot.rotation.set( #{rawJS (show $ v3d_rotX $ s3d_rotation scene3D)} , #{rawJS (show $ v3d_rotY $ s3d_rotation scene3D)} , #{rawJS (show $ v3d_rotZ $ s3d_rotation scene3D)} );
            groupRoot.scale.set( #{rawJS (show $ v3d_sclX $ s3d_scale scene3D)} , #{rawJS (show $ v3d_sclY $ s3d_scale scene3D)} , #{rawJS (show $ v3d_sclZ $ s3d_scale scene3D)} );
            scene.add(groupRoot);

            var grid = new THREE.GridHelper( 25, 50, #{rawJS (replace "#" "0x" thC_01)}, #{rawJS (replace "#" "0x" thC_02)} );
            grid.rotateOnAxis( new THREE.Vector3( 1, 0, 0 ), 90 * ( Math.PI/180 ) );
            groupRoot.add(grid);
        |]
           
        call varFn_drawRootAxes [] 

        hjs $[julius|
            renderer = new THREE.WebGLRenderer({canvas: canvas3D, antialias: #{rawJS (parsSOpts_Antialias sopts)}});
            renderer.setClearColor( #{rawJS (replace "#" "0x" thC_03)} );
            renderer.setPixelRatio( window.devicePixelRatio );
            renderer.setSize( window.innerWidth, window.innerHeight );
            renderer.sortObjects = false;
        |]


        -- | Отрисовка сцены 3D генерацией скрипта ThreeJS для
        -- создания WebGL сцены
        drawScene3D app scene3D


        -- | Управление в 3D сцене
        case parsSOpts_Control sopts of
            'o' -> hjs $[julius|
                        orbitControls = new THREE.OrbitControls(camera, renderer.domElement);
                        orbitControls.addEventListener('change', render);
                        orbitControls.target.set(0, 0, 0);
                        orbitControls.update();
                   |]
            'f' -> hjs $[julius|
                        flyControls = new THREE.FlyControls(camera);
                        flyControls.movementSpeed = 0.1;
                        flyControls.rollSpeed = Math.PI / 460;
                        flyControls.autoForward = false;
                        flyControls.dragToLook = true;
                   |]
            'n'-> return endH 
            _  -> return endH 


        -- | Статистика работы 3D сцены
        if parsSOpts_Stats sopts
        then hjs $[julius|
                stats = new Stats();
                root.appendChild(stats.dom);
             |]
        else return endH


        -- | Отработка кода скрипта из 3D модели сцены
        hjs $[julius| (function(){ #{rawJS (s3d_script scene3D)} })();  |]


        addEventListener "resize" varFn_onWindowResize False 
        call varFn_animate []

        return endH

    -- Инициализация
    call varFn_init []

    jsFinish 



-- | Отрисовка сцены 3D генерацией скрипта ThreeJS для
-- создания WebGL сцены
drawScene3D :: (CContext3D c KeyContext3D String)
            => c                    -- ^ Контекст приложения в котором генерируется сцена
            -> Scene3D              -- ^ Модель 3D сцены
            -> HSL HLangJS HLangJS  -- ^ Результат
drawScene3D app scene3D = do
    build3D app (s3d_materials scene3D) ""
    build3D app (s3d_camera    scene3D) "scene"
    build3D app (s3d_envirs    scene3D) "scene"
    build3D app (s3d_children  scene3D) "groupRoot"
    return $ hl HJsEmpty





-----------------------------------------------------------------------------------------------
-- Параметры сцены   --------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------

-- 1) o, f, n       - Controls: OrbitControls, FlyControls, None
-- 2) t, f          - Antialias: True, False
-- 3) (s | t), _    - Show stats: True, False


-- | Парсим параметры сцены
-- Управление: o - OrbitControls
--             n - None
parsSOpts_Control :: String
                  -> Char
parsSOpts_Control []    = 'o'
parsSOpts_Control (x:_) = toLower x



-- | Парсим параметры сцены
-- Сглаживание: t - true
--              _ - false
parsSOpts_Antialias :: String
                    -> String
parsSOpts_Antialias []        = "false"
parsSOpts_Antialias (_:[])    = "false"
parsSOpts_Antialias (_:(x:_)) = 
    let f = toLower x in
    if f == 't' then "true" else "false"



-- | Парсим параметры сцены
-- Stats: s - Stats
--        n - None
parsSOpts_Stats :: String
                -> Bool
parsSOpts_Stats [] = False
parsSOpts_Stats sopts = 
    if (length sopts) > 2
    then case toLower (sopts!!2) of
        's' -> True
        't' -> True
        _   -> False
    else False



