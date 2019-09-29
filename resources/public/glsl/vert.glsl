varying vec3 vNormal, vNormalPos;
varying vec2 vUv; //yo
varying vec4 vProjCoord, vModelM;
uniform mat4 textureMatrix;
const mat4 biasMatrix = mat4(0.5, 0.0, 0.0, 0.0,
                             0.0, 0.5, 0.0, 0.0,
                             0.0, 0.0, 0.5, 0.0,
                             0.5, 0.5, 0.5, 2.0); //something really fucked up is happening. sometomes this works, sometimes offset.
                             //0.5, 0.5, 0.5, 1.0); //much better but STILL not 1:1 wtf!!

void main() {
  vUv = uv;
  vNormal		   = normalize(normalMatrix * normal);
  vNormalPos   = normalize(normalMatrix * (cameraPosition - position));

  vModelM	     = modelMatrix * vec4(position, 1.0);
  /* vProjCoord   = biasMatrix * textureMatrix * vModelM; */
  vProjCoord   = textureMatrix * vModelM;
  /* vec3 crunk  = vec3(sin(position.x), position.z, position.y); */
  /* gl_Position	 = projectionMatrix * modelViewMatrix * vec4(crunk, 1.0); */
  gl_Position	 = projectionMatrix * modelViewMatrix * vec4(position, 1.0);
}
