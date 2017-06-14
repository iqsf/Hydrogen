# Hydrogen

## Introduction
Library for generating a WebGL scene by using [ThreeJS](https://threejs.org) on the web for the Haskell programming language.

## Short description
The **Hydrogen** uses the [HFitUI](http://hackage.haskell.org/package/HFitUI) library as the result of generating a WebGL scene. The WebGL scene is generated from a YAML file. This approach greatly simplifies the process of generating a WebGL scene in a Haskell with [HFitUI](http://hackage.haskell.org/package/HFitUI) library.
Below is an example generation scene from [YAML file](https://github.com/iqsf/Hydrogen/blob/master/models/test/MyTest.scene) code:


```
/*Generate by HScript from the HFitUI library*/
//Автоматически сгенерированный скрипт
//Приложение Adamant (0.0.1)
if(!Detector.webgl) Detector.addGetWebGLMessage();
 
var camera;
var scene;
var groupRoot;
var root = document.getElementById("idRoot");
var canvas3D = document.getElementById("idCanvas");
var renderer;
var stats;
var orbitControls = null;
var flyControls = null;
 
function render (){
  renderer.render(scene, camera); 
}
 
function update (){
  if(stats) stats.update(); 
  if(flyControls) flyControls.update(1); 
}
 
function animate (){
  requestAnimationFrame(animate); 
  render();
  update();
}
 
function onWindowResize (){
  if (camera) {
      camera.aspect = window.innerWidth / (window.innerHeight);
      camera.updateProjectionMatrix();
  }
  renderer.setSize( window.innerWidth, window.innerHeight);
  render();
}
 
function drawRootAxes (){
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
}
 
function init (){
  scene = new THREE.Scene();
  groupRoot = new THREE.Group();
  groupRoot.position.set( 0.0 , 0.0 , 0.0 );
  groupRoot.rotation.set( 0.0 , 0.0 , 0.0 );
  groupRoot.scale.set( 1.0 , 1.0 , 1.0 );
  scene.add(groupRoot);
  var grid = new THREE.GridHelper( 25, 50, 0x586e75, 0x073642 );
  grid.rotateOnAxis( new THREE.Vector3( 1, 0, 0 ), 90 * ( Math.PI/180 ) );
  groupRoot.add(grid);
  drawRootAxes();
  renderer = new THREE.WebGLRenderer({canvas: canvas3D, antialias: false});
  renderer.setClearColor( 0x002b36 );
  renderer.setPixelRatio( window.devicePixelRatio );
  renderer.setSize( window.innerWidth, window.innerHeight );
  renderer.sortObjects = false;
  var material4466976ede5111e6a5d3f3eee64202b6 = new THREE.MeshLambertMaterial( { color: 0x6e6e6e, emissive: 0x6e6e6e, emissiveIntensity: 0.5, opacity: 1.0, transparent: false } ); 
  (function(material){  })( material4466976ede5111e6a5d3f3eee64202b6 ); 
  var materiale6f1bd16e2fb11e687c71772a2fe4228 = new THREE.MeshLambertMaterial( { color: 0xa41a, emissive: 0x6e06e, emissiveIntensity: 0.0, opacity: 1.0, transparent: false } ); 
  (function(material){  })( materiale6f1bd16e2fb11e687c71772a2fe4228 ); 
  var material4e181b50e3bc11e6924d8bbeaf59f608 = new THREE.MeshBasicMaterial( { map: new THREE.TextureLoader().load("/texture_3d/3165d44e-e398-11e6-aac6-e7c59ca562a2") , opacity: 1.0, transparent: false } ); 
  (function(material){  })( material4e181b50e3bc11e6924d8bbeaf59f608 ); 
  var material7badc81ae6b611e69cc12b72c18ca176 = new THREE.MeshFaceMaterial([ new THREE.MeshBasicMaterial( { map: THREE.ImageUtils.loadTexture("/texture_3d/6b6fc102-e462-11e6-88eb-433f284ad9f1?nm=0"), side: THREE.BackSide } )
                                                                         , new THREE.MeshBasicMaterial( { map: THREE.ImageUtils.loadTexture("/texture_3d/6b6fc102-e462-11e6-88eb-433f284ad9f1?nm=1"), side: THREE.BackSide } )
                                                                         , new THREE.MeshBasicMaterial( { map: THREE.ImageUtils.loadTexture("/texture_3d/6b6fc102-e462-11e6-88eb-433f284ad9f1?nm=2"), side: THREE.BackSide } )
                                                                         , new THREE.MeshBasicMaterial( { map: THREE.ImageUtils.loadTexture("/texture_3d/6b6fc102-e462-11e6-88eb-433f284ad9f1?nm=3"), side: THREE.BackSide } )
                                                                         , new THREE.MeshBasicMaterial( { map: THREE.ImageUtils.loadTexture("/texture_3d/6b6fc102-e462-11e6-88eb-433f284ad9f1?nm=4"), side: THREE.BackSide } )
                                                                         , new THREE.MeshBasicMaterial( { map: THREE.ImageUtils.loadTexture("/texture_3d/6b6fc102-e462-11e6-88eb-433f284ad9f1?nm=5"), side: THREE.BackSide } )
                                                                         ]); 
         
  (function(material){  })( material7badc81ae6b611e69cc12b72c18ca176 ); 
  camera = new THREE.PerspectiveCamera( 45.0, window.innerWidth / window.innerHeight, 1.0, 1000.0 );
  camera.up.set( 0.0, 0.0, 1.0);
  camera.position.set( 0.0 , -10.0 , 10.0 );
  scene.add(camera);
  var lighte436c6c8f1d511e6837cafadb174b93a = new THREE.PointLight( 0xFFFFFF , 0.8 , 0.0 , 1.0); 
  lighte436c6c8f1d511e6837cafadb174b93a.position.set( 0.0 , 0.0 , 0.0 ); 
  (function(light){  })( lighte436c6c8f1d511e6837cafadb174b93a ); 
  camera.add(lighte436c6c8f1d511e6837cafadb174b93a); 
  var light9ab6f73cf1d211e6ad6c23968912c3bd = new THREE.AmbientLight( 0x999999 , 1.0 ); 
  (function(light){  })( light9ab6f73cf1d211e6ad6c23968912c3bd ); 
  scene.add(light9ab6f73cf1d211e6ad6c23968912c3bd); 
  var figure5d2f5584e6b611e6b1e007ba4b916aa0 = new THREE.Mesh( new THREE.BoxGeometry( 500.0 , 500.0 , 500.0 ) , material7badc81ae6b611e69cc12b72c18ca176 ); 
  figure5d2f5584e6b611e6b1e007ba4b916aa0.position.set( 0.0 , 0.0 , 0.0 ); 
  figure5d2f5584e6b611e6b1e007ba4b916aa0.rotation.set( 1.570796 , 0.0 , 0.0 ); 
  figure5d2f5584e6b611e6b1e007ba4b916aa0.scale.set( 1.0 , 1.0 , 1.0 ); 
  (function(figure, material){  })( figure5d2f5584e6b611e6b1e007ba4b916aa0 , material7badc81ae6b611e69cc12b72c18ca176 ); 
  scene.add(figure5d2f5584e6b611e6b1e007ba4b916aa0); 
  var group20c55aece15e11e69b09a74605bb9055 = new THREE.Group(); 
  group20c55aece15e11e69b09a74605bb9055.position.set( 0.0 , 0.0 , 0.0 ); 
  group20c55aece15e11e69b09a74605bb9055.rotation.set( 0.0 , 0.0 , 0.0 ); 
  group20c55aece15e11e69b09a74605bb9055.scale.set( 1.0 , 1.0 , 1.0 ); 
  (function(figure){  })( group20c55aece15e11e69b09a74605bb9055 );  
  groupRoot.add(group20c55aece15e11e69b09a74605bb9055); 
  var params1743e5eaf2a911e69ed17714401e4924 = ["Test","1.0"]; 
   var figure1743e5eaf2a911e69ed17714401e4924 = (function(material, params){ var geometry = new THREE.SphereGeometry( 1, 32, 32 );
  var sphere = new THREE.Mesh( geometry, material );   
  return sphere;
   })( material4466976ede5111e6a5d3f3eee64202b6, params1743e5eaf2a911e69ed17714401e4924 ); 
  figure1743e5eaf2a911e69ed17714401e4924.position.set( 0.0 , 4.0 , 0.0 ); 
  figure1743e5eaf2a911e69ed17714401e4924.rotation.set( 0.0 , 0.0 , 0.0 ); 
  figure1743e5eaf2a911e69ed17714401e4924.scale.set( 1.0 , 1.0 , 1.0 ); 
  (function(figure, material){  })( figure1743e5eaf2a911e69ed17714401e4924 , material4466976ede5111e6a5d3f3eee64202b6 ); 
  group20c55aece15e11e69b09a74605bb9055.add(figure1743e5eaf2a911e69ed17714401e4924); 
  var entityCamera45d7b56aedf111e6be1d539a6d03a9c1 = (function(){ 
  var video = document.createElement( 'video' );
  video.id = 'VID_45d7b56a-edf1-11e6-be1d-539a6d03a9c1';
  video.src = "/video/sintel.ogv";
  video.loop = true;
  video.muted = true;
  video.load();
  video.play();
  var videoTexture = new THREE.VideoTexture( video );
  videoTexture.minFilter = THREE.LinearFilter;
  videoTexture.magFilter = THREE.LinearFilter;
  videoTexture.format = THREE.RGBFormat;
  var movieMaterial = new THREE.MeshBasicMaterial( { map: videoTexture, overdraw: true, side:THREE.FrontSide } ); 
  movieMaterial.side = THREE.BackSide;   
  var entityGroup = new THREE.Group(); 
  entityGroup.position.set( 0.0 , 0.0 , 0.0 ); 
  entityGroup.rotation.set( 0.0 , 1.57 , 0.0 ); 
  entityGroup.scale.set( 1.0 , 1.0 , 1.0 ); 
  var meshCamera = new THREE.Mesh( new THREE.ConeGeometry(1*0.5, 1, 4), new THREE.MeshBasicMaterial({color: 0xFF0000, wireframe: true}) );
  meshCamera.rotation.x = -Math.PI/2;
  meshCamera.scale.x = meshCamera.scale.y = meshCamera.scale.z = 0.35;
  entityGroup.add(meshCamera);
  var area_0 = [ new THREE.Vector2( 0.0 , 1.0 )
                                 , new THREE.Vector2( 0.5 , 1.0 )
                                 , new THREE.Vector2( 0.5 , 0.0 )
                                 , new THREE.Vector2( 0.0 , 0.0 )
                                 ];
  var movieGeometry_0 = new THREE.PlaneGeometry(2.5, 2.5);
  movieGeometry_0.faceVertexUvs[0][0] = [ area_0[0], area_0[3], area_0[1] ];
  movieGeometry_0.faceVertexUvs[0][1] = [ area_0[3], area_0[2], area_0[1] ];
  var movieScreen_0 = new THREE.Mesh( movieGeometry_0, movieMaterial );
  movieScreen_0.geometry.vertices[0].x = movieScreen_0.geometry.vertices[0].x + 0.0
  movieScreen_0.geometry.vertices[0].y = movieScreen_0.geometry.vertices[0].y + 0.0
  movieScreen_0.geometry.vertices[0].z = movieScreen_0.geometry.vertices[0].z + 0.0
  movieScreen_0.geometry.vertices[1].x = movieScreen_0.geometry.vertices[1].x + 0.0
  movieScreen_0.geometry.vertices[1].y = movieScreen_0.geometry.vertices[1].y + 0.0
  movieScreen_0.geometry.vertices[1].z = movieScreen_0.geometry.vertices[1].z + 0.0
  movieScreen_0.geometry.vertices[2].x = movieScreen_0.geometry.vertices[2].x + 0.0
  movieScreen_0.geometry.vertices[2].y = movieScreen_0.geometry.vertices[2].y + 0.0
  movieScreen_0.geometry.vertices[2].z = movieScreen_0.geometry.vertices[2].z + 0.0
  movieScreen_0.geometry.vertices[3].x = movieScreen_0.geometry.vertices[3].x + 0.0
  movieScreen_0.geometry.vertices[3].y = movieScreen_0.geometry.vertices[3].y + 0.0
  movieScreen_0.geometry.vertices[3].z = movieScreen_0.geometry.vertices[3].z + 0.0
  movieScreen_0.position.set( 0.0 , 0.0 , 3.0 ); 
  movieScreen_0.rotation.set( 0.0 , 0.0 , 1.57 ); 
  movieScreen_0.scale.set( 1.0 , 1.0 , 1.0 ); 
  entityGroup.add(movieScreen_0);
  var area_1 = [ new THREE.Vector2( 0.5 , 1.0 )
                                 , new THREE.Vector2( 1.0 , 1.0 )
                                 , new THREE.Vector2( 1.0 , 0.0 )
                                 , new THREE.Vector2( 0.5 , 0.0 )
                                 ];
  var movieGeometry_1 = new THREE.PlaneGeometry(2.5, 2.5);
  movieGeometry_1.faceVertexUvs[0][0] = [ area_1[0], area_1[3], area_1[1] ];
  movieGeometry_1.faceVertexUvs[0][1] = [ area_1[3], area_1[2], area_1[1] ];
  var movieScreen_1 = new THREE.Mesh( movieGeometry_1, movieMaterial );
  movieScreen_1.geometry.vertices[0].x = movieScreen_1.geometry.vertices[0].x + 0.0
  movieScreen_1.geometry.vertices[0].y = movieScreen_1.geometry.vertices[0].y + 0.0
  movieScreen_1.geometry.vertices[0].z = movieScreen_1.geometry.vertices[0].z + 0.0
  movieScreen_1.geometry.vertices[1].x = movieScreen_1.geometry.vertices[1].x + 0.0
  movieScreen_1.geometry.vertices[1].y = movieScreen_1.geometry.vertices[1].y + 0.0
  movieScreen_1.geometry.vertices[1].z = movieScreen_1.geometry.vertices[1].z + 0.0
  movieScreen_1.geometry.vertices[2].x = movieScreen_1.geometry.vertices[2].x + 0.0
  movieScreen_1.geometry.vertices[2].y = movieScreen_1.geometry.vertices[2].y + 0.0
  movieScreen_1.geometry.vertices[2].z = movieScreen_1.geometry.vertices[2].z + 0.0
  movieScreen_1.geometry.vertices[3].x = movieScreen_1.geometry.vertices[3].x + 0.0
  movieScreen_1.geometry.vertices[3].y = movieScreen_1.geometry.vertices[3].y + 0.0
  movieScreen_1.geometry.vertices[3].z = movieScreen_1.geometry.vertices[3].z + 0.0
  movieScreen_1.position.set( 0.0 , 3.0 , 4.0 ); 
  movieScreen_1.rotation.set( 0.0 , 0.0 , 1.57 ); 
  movieScreen_1.scale.set( 1.0 , 1.0 , 1.0 ); 
  entityGroup.add(movieScreen_1);
  group20c55aece15e11e69b09a74605bb9055.add(entityGroup); 
  return entityGroup; 
  })(); 
  var groupc34cbde4e7b911e6a0a36bff2604ce8f = new THREE.Group(); 
  groupc34cbde4e7b911e6a0a36bff2604ce8f.position.set( 0.0 , 0.0 , 0.0 ); 
  groupc34cbde4e7b911e6a0a36bff2604ce8f.rotation.set( 0.0 , 0.0 , 0.0 ); 
  groupc34cbde4e7b911e6a0a36bff2604ce8f.scale.set( 1.0 , 1.0 , 1.0 ); 
  (function(figure){  })( groupc34cbde4e7b911e6a0a36bff2604ce8f );  
  groupRoot.add(groupc34cbde4e7b911e6a0a36bff2604ce8f); 
  orbitControls = new THREE.OrbitControls(camera, renderer.domElement);
  orbitControls.addEventListener('change', render);
  orbitControls.target.set(0, 0, 0);
  orbitControls.update();
   (function(){ if(orbitControls){
      orbitControls.minDistance = 3;
      orbitControls.maxDistance = 20;
      orbitControls.minPolarAngle = 0; 
      orbitControls.maxPolarAngle = 1.50098;
  }
   })();  
  window.addEventListener( 'resize',onWindowResize,false );
  animate();
}
init();

```




