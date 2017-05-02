----------------------------------------------------------------
-- Модуль библиотеки Hydrogen
-- Инструменты для модели данных (Tool)
-- Генератор сцены 3D 
----------------------------------------------------------------

module Hydrogen.Tool.TBuilder3D
    ( CBuilder3D         (..)
    , CBuilderModel3D    (..)
    , CBuilderFragment3D (..)
    ) where

-- Импорт модулей
import           Prelude                                     as PRL

import           Data.Char                                      (toLower)
import           Data.String.Utils                              (replace)

import           Text.Julius

import           Hydrogen.Model.MScene3D

import           Hydrogen.Tool.TScene3D

import           WebUI.HFitUI
import           WebUI.Scripts.JavaScript.HJavaScript

import           Hydrogen.HydroCore
import           Hydrogen.Model.MSceneGeneral



-----------------------------------------------------------------------------------------------
-- Материалы   --------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------

instance CBuilder3D [Material] where
    build3D _   []     parentName = do 
        endBuild
    build3D c (x:xs) parentName = do
        build3D c x  parentName
        build3D c xs parentName
        


instance CBuilder3D Material where
    build3D _ (Material _ _) _ = do
        endBuild

    -- | MaterialLambert
    build3D _ (MaterialLambert name uuid color emissive emissiveIntensity opacity transparent script) _ = do
        varName <- return $ genVarName "material" uuid
        hjs $[julius| var #{rawJS varName} = new THREE.MeshLambertMaterial( { color: #{rawJS (show color)}, emissive: #{rawJS (show emissive)}, emissiveIntensity: #{rawJS (show emissiveIntensity)}, opacity: #{rawJS (show opacity)}, transparent: #{rawJS (showBool transparent)} } ); |]
        hjs $[julius| (function(material){ #{rawJS script} })( #{rawJS varName} ); |]
        endBuild

    -- | MaterialBasic
    build3D _ (MaterialBasic name uuid texture opacity transparent script) _ = do
        varName <- return $ genVarName "material" uuid
        hjs $[julius| var #{rawJS varName} = new THREE.MeshBasicMaterial( { map: new THREE.TextureLoader().load("/texture_3d/#{rawJS texture}") , opacity: #{rawJS (show opacity)}, transparent: #{rawJS (showBool transparent)} } ); |]
        hjs $[julius| (function(material){ #{rawJS script} })( #{rawJS varName} ); |]
        endBuild

    -- | MaterialFace
    build3D _ (MaterialFace name uuid texture opacity transparent script) _ = do
        varName <- return $ genVarName "material" uuid
        hjs $[julius| var #{rawJS varName} = new THREE.MeshFaceMaterial([ new THREE.MeshBasicMaterial( { map: THREE.ImageUtils.loadTexture("/texture_3d/#{rawJS texture}?nm=0"), side: THREE.BackSide } )
                                                                        , new THREE.MeshBasicMaterial( { map: THREE.ImageUtils.loadTexture("/texture_3d/#{rawJS texture}?nm=1"), side: THREE.BackSide } )
                                                                        , new THREE.MeshBasicMaterial( { map: THREE.ImageUtils.loadTexture("/texture_3d/#{rawJS texture}?nm=2"), side: THREE.BackSide } )
                                                                        , new THREE.MeshBasicMaterial( { map: THREE.ImageUtils.loadTexture("/texture_3d/#{rawJS texture}?nm=3"), side: THREE.BackSide } )
                                                                        , new THREE.MeshBasicMaterial( { map: THREE.ImageUtils.loadTexture("/texture_3d/#{rawJS texture}?nm=4"), side: THREE.BackSide } )
                                                                        , new THREE.MeshBasicMaterial( { map: THREE.ImageUtils.loadTexture("/texture_3d/#{rawJS texture}?nm=5"), side: THREE.BackSide } )
                                                                        ]); 
        |]
        hjs $[julius| (function(material){ #{rawJS script} })( #{rawJS varName} ); |]
        endBuild





-----------------------------------------------------------------------------------------------
-- Камера   -----------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------

instance CBuilder3D Camera3D where
    build3D _ Camera3D _ = do
        endBuild

    build3D app (CameraPerspective3D name uuid position scale fov near far zoom (Vector3D upX upY upZ) script children) parentName = do
        hjs $[julius| 
            camera = new THREE.PerspectiveCamera( #{rawJS (show fov)}, window.innerWidth / window.innerHeight, #{rawJS (show near)}, #{rawJS (show far)} );
            camera.up.set( #{rawJS (show upX)}, #{rawJS (show upY)}, #{rawJS (show upZ)});
            camera.position.set( #{rawJS (show $ v3d_posX position)} , #{rawJS (show $ v3d_posY position)} , #{rawJS (show $ v3d_posZ position)} );
            scene.add(camera);
        |]
        build3D app children "camera"
        endBuild





-----------------------------------------------------------------------------------------------
-- Фигуры   -----------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------

instance CBuilder3D [Figure3D] where
    build3D _   []     parentName = do 
        endBuild
    build3D app (x:xs) parentName = do
        build3D app x  parentName
        build3D app xs parentName
        


instance CBuilder3D Figure3D where
    build3D _ Figure3D _ = do
        endBuild

    -- | Group 3D
    build3D app (FGroup3D name uuid flagDraw position rotation scale script children) parentName = 
        case flagDraw of
            True  -> do varName <- return $ genVarName "group" uuid
                        hjs $[julius| var #{rawJS varName} = new THREE.Group(); |]
                        buildPosRotScale varName position rotation scale
                        hjs $[julius| (function(figure){ #{rawJS script} })( #{rawJS varName} );  |]
                        hjs $[julius| #{rawJS parentName}.add(#{rawJS varName}); |]
                        build3D app children varName
                        endBuild
            False -> do endBuild

    -- | VGroup 3D
    build3D app (FVGroup3D name uuid flagDraw children) parentName = 
        case flagDraw of
            True  -> do build3D app children parentName 
                        endBuild
            False -> do endBuild

    -- | Model 3D
    build3D app (FModel3D name uuid flagDraw material position rotation scale model script) parentName = do
        case flagDraw of
            True  -> do model3D <- liftIO $ loadModel3D app model 
                        buildModel3D app model3D parentName (material, position, rotation, scale, script)
                        endBuild
            False -> do endBuild

    -- | Fragment 3D
    build3D app (FFragment3D name uuid flagDraw position rotation scale scene script) parentName = do
        case flagDraw of
            True  -> do scene3D <- liftIO $ loadScene3D app scene
                        buildFragment3D app scene3D parentName (uuid, position, rotation, scale, script)
                        endBuild
            False -> do endBuild

    -- | AmbientLight 3D
    build3D _ (FLAmbientLight3D name uuid flagDraw color intensity script) parentName = do
        case flagDraw of
            True  -> do varName <- return $ genVarName "light" uuid
                        hjs $[julius| var #{rawJS varName} = new THREE.AmbientLight( #{rawJS color} , #{rawJS (show intensity)} ); |]
                        hjs $[julius| (function(light){ #{rawJS script} })( #{rawJS varName} ); |]
                        hjs $[julius| #{rawJS parentName}.add(#{rawJS varName}); |]
                        endBuild
            False -> do endBuild

    -- | PointLight 3D
    build3D _ (FLPointLight3D name uuid flagDraw color intensity distance decay position script) parentName = do
        case flagDraw of
            True  -> do varName <- return $ genVarName "light" uuid
                        hjs $[julius| var #{rawJS varName} = new THREE.PointLight( #{rawJS color} , #{rawJS (show intensity)} , #{rawJS (show distance)} , #{rawJS (show decay)}); |]
                        hjs $[julius| #{rawJS varName}.position.set( #{rawJS (show $ v3d_posX position)} , #{rawJS (show $ v3d_posY position)} , #{rawJS (show $ v3d_posZ position)} ); |]
                        hjs $[julius| (function(light){ #{rawJS script} })( #{rawJS varName} ); |]
                        hjs $[julius| #{rawJS parentName}.add(#{rawJS varName}); |]
                        endBuild
            False -> do endBuild

    -- | DirectionalLight 3D
    build3D _ (FLDirectionalLight3D name uuid flagDraw color intensity position script) parentName = do
        case flagDraw of
            True  -> do varName <- return $ genVarName "light" uuid
                        hjs $[julius| var #{rawJS varName} = new THREE.DirectionalLight( #{rawJS color} , #{rawJS (show intensity)} ); |]
                        hjs $[julius| #{rawJS varName}.position.set( #{rawJS (show $ v3d_posX position)} , #{rawJS (show $ v3d_posY position)} , #{rawJS (show $ v3d_posZ position)} ); |]
                        hjs $[julius| (function(light){ #{rawJS script} })( #{rawJS varName} ); |]
                        hjs $[julius| #{rawJS parentName}.add(#{rawJS varName}); |]
                        endBuild
            False -> do endBuild

    -- | HemisphereLight 3D
    build3D _ (FLHemisphereLight3D name uuid flagDraw colorSky colorGround intensity position script) parentName = do
        case flagDraw of
            True  -> do varName <- return $ genVarName "light" uuid
                        hjs $[julius| var #{rawJS varName} = new THREE.HemisphereLight( #{rawJS colorSky} , #{rawJS colorGround} , #{rawJS (show intensity)} ); |]
                        hjs $[julius| #{rawJS varName}.position.set( #{rawJS (show $ v3d_posX position)} , #{rawJS (show $ v3d_posY position)} , #{rawJS (show $ v3d_posZ position)} ); |]
                        hjs $[julius| (function(light){ #{rawJS script} })( #{rawJS varName} ); |]
                        hjs $[julius| #{rawJS parentName}.add(#{rawJS varName}); |]
                        endBuild
            False -> do endBuild

    -- | SpotLight 3D
    build3D _ (FLSpotLight3D name uuid flagDraw color intensity distance angle penumbra decay position script) parentName = do
        case flagDraw of
            True  -> do varName <- return $ genVarName "light" uuid
                        hjs $[julius| var #{rawJS varName} = new THREE.SpotLight( #{rawJS color} , #{rawJS (show intensity)} , #{rawJS (show distance)} , #{rawJS (show angle)} , #{rawJS (show penumbra)} , #{rawJS (show decay)} ); |]
                        hjs $[julius| #{rawJS varName}.position.set( #{rawJS (show $ v3d_posX position)} , #{rawJS (show $ v3d_posY position)} , #{rawJS (show $ v3d_posZ position)} ); |]
                        hjs $[julius| (function(light){ #{rawJS script} })( #{rawJS varName} ); |]
                        hjs $[julius| #{rawJS parentName}.add(#{rawJS varName}); |]
                        endBuild
            False -> do endBuild

    -- | Box 3D
    build3D _ (FBox3D name uuid flagDraw material position rotation scale sizeX sizeY sizeZ script) parentName = do
        case flagDraw of
            True  -> do varName         <- return $ genVarName "figure"   uuid
                        varMaterialName <- return $ genVarName "material" material
                        hjs $[julius| var #{rawJS varName} = new THREE.Mesh( new THREE.BoxGeometry( #{rawJS (show sizeX)} , #{rawJS (show sizeY)} , #{rawJS (show sizeZ)} ) , #{rawJS  varMaterialName} ); |]
                        buildPosRotScale varName position rotation scale
                        hjs $[julius| (function(figure, material){ #{rawJS script} })( #{rawJS varName} , #{rawJS varMaterialName} ); |]
                        hjs $[julius| #{rawJS parentName}.add(#{rawJS varName}); |]
                        endBuild
            False -> do endBuild

    -- | Plane 3D
    build3D _ (FPlane3D name uuid flagDraw material position rotation scale sizeX sizeY script) parentName = do
        case flagDraw of
            True  -> do varName         <- return $ genVarName "figure"   uuid
                        varMaterialName <- return $ genVarName "material" material
                        hjs $[julius| var #{rawJS varName} = new THREE.Mesh( new THREE.PlaneGeometry( #{rawJS (show sizeX)} , #{rawJS (show sizeY)} ) , #{rawJS  varMaterialName} ); |]
                        buildPosRotScale varName position rotation scale
                        hjs $[julius| (function(figure, material){ #{rawJS script} })( #{rawJS varName} , #{rawJS varMaterialName} ); |]
                        hjs $[julius| #{rawJS parentName}.add(#{rawJS varName}); |]
                        endBuild
            False -> do endBuild

    -- | Script 3D
    build3D _ (FScript3D name uuid flagDraw material position rotation scale flagExt params scriptDo script) parentName = do
        case (flagDraw, flagExt) of
            (True, False) -> do varName         <- return $ genVarName "figure"   uuid
                                varMaterialName <- return $ genVarName "material" material
                                varParamsName   <- return $ genVarName "params"   uuid
                                hjs $[julius| var #{rawJS varParamsName} = #{rawJS (show params)}; |]
                                hjs $[julius| var #{rawJS varName} = (function(material, params){ #{rawJS scriptDo} })( #{rawJS varMaterialName}, #{rawJS varParamsName} ); |]
                                buildPosRotScale varName position rotation scale
                                hjs $[julius| (function(figure, material){ #{rawJS script} })( #{rawJS varName} , #{rawJS varMaterialName} ); |]
                                hjs $[julius| #{rawJS parentName}.add(#{rawJS varName}); |]
                                endBuild
            (True, True)  -> do varName         <- return $ genVarName "figure"   uuid
                                varMaterialName <- return $ genVarName "material" material
                                varParamsName   <- return $ genVarName "params"   uuid
                                hjs $[julius| var #{rawJS varParamsName} = #{rawJS (show params)}; |]

                                hjs $[julius| 
                                    var #{rawJS varName} = (function(material, params, parent, px, py, pz, rx, ry, rz, sx, sy, sz){ #{rawJS scriptDo} 
                                    })( #{rawJS varMaterialName}, #{rawJS varParamsName}, #{rawJS parentName}
                                      , #{rawJS (show $ v3d_posX position)} , #{rawJS (show $ v3d_posY position)} , #{rawJS (show $ v3d_posZ position)}
                                      , #{rawJS (show $ v3d_rotX rotation)} , #{rawJS (show $ v3d_rotY rotation)} , #{rawJS (show $ v3d_rotZ rotation)}
                                      , #{rawJS (show $ v3d_sclX scale)} , #{rawJS (show $ v3d_sclY scale)} , #{rawJS (show $ v3d_sclZ scale)}
                                      );
                                |]

                                hjs $[julius| (function(figure, material){ #{rawJS script} })( #{rawJS varName} , #{rawJS varMaterialName} ); |]
                                endBuild
            (False, _)    -> do endBuild

    -- | EntityCamera 3D 
    build3D _ (FEntityCamera3D name uuid flagDraw position rotation scale params source (ECOptions3D fdFC clrC geomC scaleC) opacity scrnSide screens script) parentName = do
        case flagDraw of
            True  -> do varName      <- return $ genVarName "entityCamera" uuid
                        hjs $[julius| var #{rawJS varName} = (function(){ |]
                        
                        hjs $[julius| 
                            var video = document.createElement( 'video' );
                            video.id = 'VID_#{rawJS uuid}';
                            video.src = "#{rawJS source}";
                            video.loop = true;
                            video.muted = true;
                            video.load();
                            video.play();

                            var videoTexture = new THREE.VideoTexture( video );
                            videoTexture.minFilter = THREE.LinearFilter;
                            videoTexture.magFilter = THREE.LinearFilter;
                            videoTexture.format = THREE.RGBFormat;
                        |]

                        if opacity > 0.0 && opacity < 1.0
                        then hjs $[julius| var movieMaterial = new THREE.MeshBasicMaterial( { map: videoTexture, overdraw: true, side:THREE.FrontSide, opacity: #{rawJS (show opacity)}, transparent: true } ); |]
                        else hjs $[julius| var movieMaterial = new THREE.MeshBasicMaterial( { map: videoTexture, overdraw: true, side:THREE.FrontSide } ); |]
                        
                        case scrnSide of
                            "FrontSide"  -> do hjs $[julius| movieMaterial.side = THREE.FrontSide;  |]
                            "BackSide"   -> do hjs $[julius| movieMaterial.side = THREE.BackSide;   |]
                            "DoubleSide" -> do hjs $[julius| movieMaterial.side = THREE.DoubleSide; |]

                        hjs $[julius| var entityGroup = new THREE.Group(); |]
                        buildPosRotScale "entityGroup" position rotation scale

                        if fdFC == True
                        then hjs $[julius| 
                                var meshCamera = new THREE.Mesh( new THREE.ConeGeometry(1*#{rawJS (show geomC)}, 1, 4), new THREE.MeshBasicMaterial({color: #{rawJS clrC}, wireframe: true}) );
                                meshCamera.rotation.x = -Math.PI/2;
                                meshCamera.scale.x = meshCamera.scale.y = meshCamera.scale.z = #{rawJS (show scaleC)};
                                entityGroup.add(meshCamera);
                             |]
                        else endBuild

                        mapM_ buildECScreen3D screens

                        --hjs $[julius| var #{rawJS varName} = new THREE.Mesh( new THREE.PlaneGeometry( #{rawJS (show sizeX)} , #{rawJS (show sizeY)} ) , #{rawJS  varMaterialName} ); |]
                        --buildPosRotScale varName position rotation scale
                        --hjs $[julius| (function(figure, material){ #{rawJS script} })( #{rawJS varName} , #{rawJS varMaterialName} ); |]
                        hjs $[julius| #{rawJS parentName}.add(entityGroup); |]
                        hjs $[julius| return entityGroup; |]
                        hjs $[julius| })(); |]
                        endBuild
            False -> do endBuild
        where
            buildECScreen3D :: ECScreen3D -> HSL HLangJS ()
            buildECScreen3D (ECScreen3D nmb  szx szy  px py pz  rx ry rz  sx sy sz  dx0 dy0 dz0  dx1 dy1 dz1  dx2 dy2 dz2  dx3 dy3 dz3  ax0 ay0  ax1 ay1  ax2 ay2  ax3 ay3) = do
                hjs $[julius|
 
                    var area_#{rawJS (show nmb)} = [ new THREE.Vector2( #{rawJS (show ax0)} , #{rawJS (show ay0)} )
                                                   , new THREE.Vector2( #{rawJS (show ax1)} , #{rawJS (show ay1)} )
                                                   , new THREE.Vector2( #{rawJS (show ax2)} , #{rawJS (show ay2)} )
                                                   , new THREE.Vector2( #{rawJS (show ax3)} , #{rawJS (show ay3)} )
                                                   ];

                    var movieGeometry_#{rawJS (show nmb)} = new THREE.PlaneGeometry(2.5, 2.5);
                    movieGeometry_#{rawJS (show nmb)}.faceVertexUvs[0][0] = [ area_#{rawJS (show nmb)}[0], area_#{rawJS (show nmb)}[3], area_#{rawJS (show nmb)}[1] ];
                    movieGeometry_#{rawJS (show nmb)}.faceVertexUvs[0][1] = [ area_#{rawJS (show nmb)}[3], area_#{rawJS (show nmb)}[2], area_#{rawJS (show nmb)}[1] ];

                    var movieScreen_#{rawJS (show nmb)} = new THREE.Mesh( movieGeometry_#{rawJS (show nmb)}, movieMaterial );

                    movieScreen_#{rawJS (show nmb)}.geometry.vertices[0].x = movieScreen_#{rawJS (show nmb)}.geometry.vertices[0].x + #{rawJS (show dx0)}
                    movieScreen_#{rawJS (show nmb)}.geometry.vertices[0].y = movieScreen_#{rawJS (show nmb)}.geometry.vertices[0].y + #{rawJS (show dy0)}
                    movieScreen_#{rawJS (show nmb)}.geometry.vertices[0].z = movieScreen_#{rawJS (show nmb)}.geometry.vertices[0].z + #{rawJS (show dz0)}

                    movieScreen_#{rawJS (show nmb)}.geometry.vertices[1].x = movieScreen_#{rawJS (show nmb)}.geometry.vertices[1].x + #{rawJS (show dx1)}
                    movieScreen_#{rawJS (show nmb)}.geometry.vertices[1].y = movieScreen_#{rawJS (show nmb)}.geometry.vertices[1].y + #{rawJS (show dy1)}
                    movieScreen_#{rawJS (show nmb)}.geometry.vertices[1].z = movieScreen_#{rawJS (show nmb)}.geometry.vertices[1].z + #{rawJS (show dz1)}

                    movieScreen_#{rawJS (show nmb)}.geometry.vertices[2].x = movieScreen_#{rawJS (show nmb)}.geometry.vertices[2].x + #{rawJS (show dx2)}
                    movieScreen_#{rawJS (show nmb)}.geometry.vertices[2].y = movieScreen_#{rawJS (show nmb)}.geometry.vertices[2].y + #{rawJS (show dy2)}
                    movieScreen_#{rawJS (show nmb)}.geometry.vertices[2].z = movieScreen_#{rawJS (show nmb)}.geometry.vertices[2].z + #{rawJS (show dz2)}

                    movieScreen_#{rawJS (show nmb)}.geometry.vertices[3].x = movieScreen_#{rawJS (show nmb)}.geometry.vertices[3].x + #{rawJS (show dx3)}
                    movieScreen_#{rawJS (show nmb)}.geometry.vertices[3].y = movieScreen_#{rawJS (show nmb)}.geometry.vertices[3].y + #{rawJS (show dy3)}
                    movieScreen_#{rawJS (show nmb)}.geometry.vertices[3].z = movieScreen_#{rawJS (show nmb)}.geometry.vertices[3].z + #{rawJS (show dz3)}

                |]

                buildPosRotScale ("movieScreen_" ++ (show nmb)) (Position3D px py pz) (Rotation3D rx ry rz) (Scale3D sx sy sz)

                hjs $[julius|
                    entityGroup.add(movieScreen_#{rawJS (show nmb)});

                |]
                return () 





-----------------------------------------------------------------------------------------------
-- Модели   -----------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------

instance CBuilderModel3D [Model3D] where
    buildModel3D _   []     parentName _ = do 
        endBuild
    buildModel3D app (x:xs) parentName mprss = do
        buildModel3D app x  parentName mprss
        buildModel3D app xs parentName mprss
        


instance CBuilderModel3D Model3D where
    buildModel3D _ Model3D _ _ = do
        endBuild

    buildModel3D _ (Model3D_Obj name uuid version author descr tags cntObjs cntMtls) parentName (material, position, rotation, scale, script) = 
        if cntMtls == []
        then do varMaterialName <- return $ genVarName "material" material
                hjs $[julius| (function(material){ |]
                hjs $[julius| 
                    var objLoader = new THREE.OBJLoader();
                    objLoader.load("/model_3d/obj/#{rawJS uuid}", function(object) {
                    object.traverse(function(child) {
                        if (child instanceof THREE.Mesh){
                            child.material = material;
                        }
                    });
                |]
                buildPosRotScale "object" position rotation scale
                hjs $[julius| (function(figure){ #{rawJS script} })( object ); |]
                hjs $[julius| 
                        ;#{rawJS parentName}.add(object);
                    });
                |]
                hjs $[julius| })(#{rawJS varMaterialName}); |]
                endBuild
        else do hjs $[julius| (function(){ |]
                hjs $[julius| 
                    var mtlLoader = new THREE.MTLLoader();
                    mtlLoader.load("/model_3d/mtl/#{rawJS uuid}", function(materials) {
                        materials.preload();
                        var objLoader = new THREE.OBJLoader();
                        objLoader.setMaterials(materials);
                        objLoader.load("/model_3d/obj/#{rawJS uuid}", function(object) {
                |]
                buildPosRotScale "object" position rotation scale
                hjs $[julius| (function(figure){ #{rawJS script} })( object ); |]
                hjs $[julius| 
                            ;#{rawJS parentName}.add(object);
                        });
                    });
                |]
                hjs $[julius| })(); |]
                endBuild

    buildModel3D _ (Model3D_Json name uuid version author descr tags contants) parentName (material, position, rotation, scale, script) = do 
        varMaterialName <- return $ genVarName "material" material
        hjs $[julius| (function(){ |]
        hjs $[julius| 
            var jsonLoader = new THREE.JSONLoader();
            jsonLoader.load("/model_3d/json/#{rawJS uuid}", function(geometry) {
            var mesh = new THREE.Mesh(geometry, #{rawJS varMaterialName});
        |]
        buildPosRotScale "mesh" position rotation scale
        hjs $[julius| (function(figure){ #{rawJS script} })( mesh ); |]
        hjs $[julius| 
                ;#{rawJS parentName}.add(mesh);
            });
        |]
        hjs $[julius| })(); |]
        endBuild





-----------------------------------------------------------------------------------------------
-- Фрагменты   --------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------

instance CBuilderFragment3D [Scene3D] where
    buildFragment3D _   []     parentName _ = do 
        endBuild
    buildFragment3D app (x:xs) parentName fprss = do
        buildFragment3D app x  parentName fprss
        buildFragment3D app xs parentName fprss
        


instance CBuilderFragment3D Scene3D where
    buildFragment3D app scene3D parentName (uuid, position, rotation, scale, script) = do
        uuidScene <- return $ s3d_uuid scene3D
        varName   <- return $ genVarName "fragment" uuid
        varFSName <- return $ genVarName "fragment" uuidScene
        hjs $[julius| /* FragmentBegin. UUID: #{rawJS uuid} */  |]
        hjs $[julius| var #{rawJS varName} = (function(){ |]
        hjs $[julius| var #{rawJS varFSName} = new THREE.Group(); |]
        buildPosRotScale varFSName position rotation scale
        build3D app (s3d_materials scene3D) ""
        build3D app (s3d_children  scene3D) varFSName 
        hjs $[julius| return #{rawJS varFSName}; })(); |]
        hjs $[julius| #{rawJS parentName}.add(#{rawJS varName}); |]
        hjs $[julius| /* FragmentEnd. UUID: #{rawJS uuid} */  |]
        endBuild





-----------------------------------------------------------------------------------------------
-- Вспомогательные инструменты   --------------------------------------------------------------
-----------------------------------------------------------------------------------------------

-- | Генерация имени переменной
genVarName :: String 
           -> UUID_3D 
           -> String
genVarName prefix uuid =
    if uuid == ""
    then "null"
    else let p = replace "-" "" uuid in
         prefix ++ p



-- | Генерация имени переменной материала
genVarMatName :: UUID_3D 
              -> String
genVarMatName uuid = genVarName "material" uuid



-- | Генерация имени переменной фмгуры
genVarFigName :: UUID_3D 
              -> String
genVarFigName uuid = genVarName "figure" uuid



-- | Перевод Bool в строку для скрипта
showBool :: Bool
         -> String
showBool flag = 
    map toLower $ show flag



-- | Окончание построения сцены
buildPosRotScale :: String
                 -> Position3D
                 -> Rotation3D
                 -> Scale3D
                 -> HSL HLangJS HLangJS
buildPosRotScale varName position rotation scale = do
    hjs $[julius| #{rawJS varName}.position.set( #{rawJS (show $ v3d_posX position)} , #{rawJS (show $ v3d_posY position)} , #{rawJS (show $ v3d_posZ position)} ); |]
    hjs $[julius| #{rawJS varName}.rotation.set( #{rawJS (show $ v3d_rotX rotation)} , #{rawJS (show $ v3d_rotY rotation)} , #{rawJS (show $ v3d_rotZ rotation)} ); |]
    hjs $[julius| #{rawJS varName}.scale.set( #{rawJS (show $ v3d_sclX scale)} , #{rawJS (show $ v3d_sclY scale)} , #{rawJS (show $ v3d_sclZ scale)} ); |]
    



-- | Окончание построения сцены
endBuild :: HSL HLangJS HLangJS
endBuild = do
    return $ hl HJsEmpty



