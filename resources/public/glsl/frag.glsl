varying vec2  vUv;
varying vec3	vNormal, vNormalPos;
varying vec4  vProjCoord, vModelM;

uniform sampler2D tDiffuse, tDepth;
uniform float near, far;                   // (likely) camera near/far, for depth calculations
uniform vec3		lightColor, spotPosition;
uniform float	furthest, anglePower, opacity, cap, dimmer;
//cap maybe better as a #define

float toLinearDepth(float z) {      // [0,1]
  float ndcZ = 2.0 * z - 1.0;       // [-1,1]
  float linearDepth = (2.0 * near * far) / (far + near - ndcZ * (far - near));
  return linearDepth; // / far;   // (not) back to [0,1] unless div by far but eh resolution?
  /* return linearDepth / far;   // (not) back to [0,1] unless div by far but eh resolution? */
}

void main() { // wishlist: fix angle thing, moving fog noise texture, 
  // "black" cant exist as color - make transparent
  /* float depth    = readDepth(tDepth, vUv); */
  //vec3 diffuse   = texture2D(tDiffuse, vUv).rgb;

  if(dimmer < 0.01) {
    gl_FragColor	     = vec4(vec3(0.0), 0.0); //skip
  } else {
    float distanceFade = distance(vModelM.xyz, spotPosition) / furthest; // attenuation by distance
    float intensity	   = cap - clamp(distanceFade, 0.0, cap);            // only place a hard cap works is here

    vec3 normal	  = vec3(vNormal.x, vNormal.y, abs(vNormal.z));            // so normalized normal, abs z bc why?
    vec3 straight = vec3(0.0, 0.0, 1.0);
    /* float angleIntensity	= pow(abs(dot(vNormal, vNormalPos)), anglePower); //this gets weird when facing */
    float angleIntensity	= pow(abs(dot(normal, straight)), anglePower); //this gets weird when facing

    float radius      = 1.515;
    float ourDepth    = toLinearDepth(gl_FragCoord.z);
    float bufferDepth = toLinearDepth(texture2DProj(tDepth, vProjCoord).x); //correct
    float diff        = clamp(abs(ourDepth - bufferDepth) / radius, 0.0, 1.0); // 'how close to anything solid?'
    /* float fadeNearNear = clamp(ourDepth, 0.0, 1.0);     // not really working. something like this, when our depth v low (cone near camera near plane) fade out so dont pop in face. else let be. */
    /* intensity	        = dimmer * fadeNearNear * diff * opacity * intensity * angleIntensity; */
    intensity	        = dimmer * diff * opacity * intensity * angleIntensity;

    gl_FragColor	     = vec4(lightColor, intensity);                         // set the final color
    /* gl_FragColor	     = vec4(vec3(fadeNearNear), 0.8);                         // set the final color */
    gl_FragColor	     = vec4(vec3(diff), 0.8);                         // set the final color
    /* gl_FragColor	     = vec4(vec3(ourDepth / far), 0.8);                         // set the final color */
    /* gl_FragColor	     = vec4(vec3(bufferDepth / far), 0.8);                         // set the final color */
    /* gl_FragColor	     = vec4(vec3(diff*angleIntensity), 0.8);                         // set the final color */
  }
}
