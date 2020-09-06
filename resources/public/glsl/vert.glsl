varying vec3 vNormal, vNormalPos, vActualNormal;
varying vec2 vUv;
varying vec4 vProjCoord, vModelM;
uniform vec3 viewVector;
uniform float time;
const mat4 biasMatrix = mat4(0.5, 0.0, 0.0, 0.0,
                             0.0, 0.5, 0.0, 0.0,
                             0.0, 0.0, 0.5, 0.0,
                             0.5, 0.5, 0.5, 2.0); //something really fucked up is happening. sometomes this works, sometimes offset.

void main() {
  vUv = uv;
  vNormal		   = normalize(normalMatrix * normal);
  /* vNormalPos   = normalize(normalMatrix * (cameraPosition - position)); */
  /* vActualNormal = vec3(modelMatrix * vec4(normal, 0.0)); */
  vActualNormal = normalize(vec3(modelMatrix * vec4(normal, 0.0)));
  vNormalPos   = normalize(cameraPosition - position);
  /* vNormalPos   = normalize(cameraPosition - vec3(modelMatrix * vec4(position, 1.0))); */

  vModelM	     = modelMatrix * vec4(position, 1.0);
  /* vec3 crunk  = vec3(position.x, sin(time) * 0.2 +position.y, position.z); */
  /* gl_Position	 = projectionMatrix * modelViewMatrix * vec4(crunk, 1.0); */
  gl_Position	 = projectionMatrix * modelViewMatrix * vec4(position, 1.0);
  vProjCoord   = biasMatrix * gl_Position;
}
