varying vec2  vUv;
varying vec3	vNormal, vNormalPos, vActualNormal;
varying vec4  vProjCoord, vModelM;

uniform sampler2D tDiffuse, tDepth, tFogNoise;
uniform float near, far;                   // (likely) camera near/far, for depth calculations
uniform vec3	lightColor, spotPosition;
uniform float time;
uniform float	furthest, anglePower, opacity, cap, dimmer;
//cap maybe better as a #define

float toLinearDepth(float z) {      // [0,1]
  float ndcZ = 2.0 * z - 1.0;       // [-1,1]
  float linearDepth = (2.0 * near * far) / (far + near - ndcZ * (far - near));
  return linearDepth; // / far;   // (not) back to [0,1] unless div by far but eh resolution?
  /* return linearDepth / far; // / far;   // (not) back to [0,1] unless div by far but eh resolution? */
}


vec3 hash33(vec3 p){ // Hash function. This particular one probably doesn't disperse things quite
    float n = sin(dot(p, vec3(7, 157, 113)));
    return fract(vec3(2097152, 262144, 32768)*n);
}

// IQ's texture lookup noise... in obfuscated form. There's less writing, so
// that makes it faster. That's how optimization works, right? :) Seriously,
// though, refer to IQ's original for the proper function.
// By the way, you could replace this with the non-textured version, and the shader should run at almost the same efficiency.
float pn( in vec3 p ){
  vec3 i = floor(p); p -= i; p *= p*(3. - 2.*p);
	p.xy = texture2D(tFogNoise, (p.xy + i.xy + vec2(37, 17)*i.z + .5)/256., -100.).yx;
	return mix(p.x, p.y, p.z);
}

// Basic low quality noise consisting of three layers of rotated, mutated
// trigonometric functions. Needs work, but sufficient for this example.
float trigNoise3D(in vec3 p){
    float res = 0., sum = 0.;
    // IQ's cheap, texture-lookup noise function. Very efficient, but still
    // a little too processor intensive for multiple layer usage in a largish
    // "for loop" setup. Therefore, just one layer is being used here.
    float n = pn(p*8. + time*2.);
    // Two sinusoidal layers. I'm pretty sure you could get rid of one of
    // the swizzles (I have a feeling the GPU doesn't like them as much),
    // which I'll try to do later.
    vec3 t = sin(p.yzx*3.14159265 + cos(p.zxy*3.14159265+1.57/2.))*0.5 + 0.5;
    p = p*1.5 + (t - 1.5); //  + time*0.1
    res += (dot(t, vec3(0.333)));

    t = sin(p.yzx*3.14159265 + cos(p.zxy*3.14159265+1.57/2.))*0.5 + 0.5;
    res += (dot(t, vec3(0.333)))*0.7071;

	return ((res/1.7071))*0.85 + n*0.15;
}

// Distance function.
float map(vec3 p) {
    return trigNoise3D(p*0.5);
    // Three layers of noise, for comparison.
    //p += time;
    //return pn(p*.75)*0.57 + pn(p*1.875)*0.28 + pn(p*4.6875)*0.15;
}


/* vec4 mainImage() {  // Unit direction ray vector: Note the absence of a divide term. I came across this via a comment Shadertoy user "coyote" made. I'm pretty easy to please, but I thought it was pretty cool. */
/*     /1* vec3 rd = normalize(vec3(fragCoord - iResolution.xy*.5, iResolution.y*.75)); *1/ */
/*     vec3 rd = normalize(vec3(gl_FragCoord.xy - vUv.xy*.5, vUv.y*.75)); */
/*     vec3 ro = vec3(0, 0, time*4.); // Ray origin. Moving along the Z-axis. */

/*     // The ray is effectively marching through discontinuous slices of noise, so at certain */
/*     // angles, you can see the separation. A bit of randomization can mask that, to a degree. */
/*     // At the end of the day, it's not a perfect process. Note, the ray is deliberately left */
/*     // unnormalized... if that's a word. */
/*     rd = (rd + (hash33(rd.zyx)*.006 - .003)); //Randomizing the direction. */
/*     rd *= (1. + fract(sin(dot(vec3(7, 157, 113), rd.zyx))*43758.5453)*0.06-0.03);      // Randomizing the length also. */

/*     float lDe = 0., td = 0., w = 0.; // Local density, total density, and weighting factor. */
/*     float d = 1., t = 0.; // Closest surface distance, and total ray distance travelled. */
/*     const float h = .5; // Distance threshold. Higher numbers give thicker clouds, but fill up the screen too much. */
/*     vec3 col = vec3(0), sp; // Initializing the scene color to black, and declaring the surface position vector. */

/*     // Particle surface normal. */
/*     // Here's my hacky reasoning. I'd imagine you're going to hit the particle front on, so the normal */
/*     // would just be the opposite of the unit direction ray. However particles are particles, so there'd */
/*     // be some randomness attached... Yeah, I'm not buying it either. :) */
/*     vec3 sn = normalize(hash33(rd.yxz)*.03-rd); */

/*     for (int i=0; i<64; i++) { // Raymarching loop. */
/*         if((td>1.) || d<.001*t || t>80.) break; //Loop break conditions. Seems to work, but let me know if I've overlooked something. */

/*         sp = ro + rd*t; // Current ray position. */
/*         d = map(sp); // Closest distance to the surface... particle. */

/*         // If we get within a certain distance, "h," of the surface, accumulate some surface values. */
/*         // The "step" function is a branchless way to do an if statement, in case you're wondering. */
/*         // */
/*         // Values further away have less influence on the total. When you accumulate layers, you'll */
/*         // usually need some kind of weighting algorithm based on some identifying factor - in this */
/*         // case, it's distance. This is one of many ways to do it. In fact, you'll see variations on */
/*         // the following lines all over the place. */
/*         lDe = (h - d)*step(d, h); */
/*         w = (1. - td) * lDe; */

/*         // Use the weighting factor to accumulate density. How you do this is up to you. */
/*         td += w*w*8. + 1./64.; //w*w*5. + 1./50.; */
/*         //td += w*.4 + 1./45.; // Looks cleaner, but a little washed out. */

/*         // Point light calculations. */

/*         vec3 ld = lp - sp; // Direction vector from the surface to the light position. */
/*         float lDist = max(length(ld), .001); // Distance from the surface to the light. */
/*         ld/=lDist; // Normalizing the directional light vector. */

/*         float atten = 1./(1. + lDist*.125 + lDist*lDist*.05); // Using the light distance to perform some falloff. */

/*         // Ok, these don't entirely correlate with tracing through transparent particles, */
/*         // but they add a little anglular based highlighting in order to fake proper lighting... */
/*         // if that makes any sense. I wouldn't be surprised if the specular term isn't needed, or could be taken outside the loop. */
/*         float diff = max(dot( sn, ld ), 0.); */
/*         float spec = pow(max( dot( reflect(-ld, sn), -rd ), 0. ), 4.); */

/*         // Accumulating the color. Note that I'm only adding a scalar value, in this case, */
/*         // but you can add color combinations. Note the "d*3. - .1" term. It's there as a bit */
/*         // of a fudge to make the clouds a bit more shadowy. */
/*         col += w*(d*3. - .1)*(.5 + diff + spec*.5)*atten; */

/*         // Try this instead, to see what it looks like without the fake contrasting. Obviously, much faster. */
/*         //col += w*atten*1.25; */

/*         // Enforce minimum stepsize. This is probably the most important part of the procedure. */
/*         // It reminds me a little of of the soft shadows routine. */
/*         t +=  max(d*.5, .02); // */
/*         // t += .2; // t += d*.5;// These also work, but don't seem as efficient. */
/*     } */

/*     col = max(col, 0.); */

/*     // trigNoise3D(rd*1.) */
/*     col = mix(pow(vec3(1.5, 1, 1)*col,  vec3(1, 2, 8)), col, dot(cos(rd*6. +sin(rd.yzx*6.)), vec3(.333))*.35 + .65); */
/*     col = mix(col.zyx, col, dot(cos(rd*9. +sin(rd.yzx*9.)), vec3(.333))*.15 + .85);//xzy */
/*     //col = mix(col.zyx, col, dot(rd, vec3(.5))+.5); */
/*     return vec4(sqrt(max(col, 0.)), 1.0); */
/* } */

float hash(in vec3 p) {
	return fract(sin(dot(p,vec3(283.6,127.1,311.7))) * 43758.5453);
}

float noise(vec3 p, vec3 fft, vec3 wav) {
	p.y -= time * 2. + 2. * fft.x * fft.y;
	p.z += time * .4 - fft.z;
	p.x += 2. * cos(wav.y);

  vec3 i = floor(p);
	vec3 f = fract(p);
	f *= f * (3. - 2. * f);

  vec2 c = vec2(0,1);

  return mix(
		mix(mix(hash(i + c.xxx), hash(i + c.yxx),f.x),
        mix(hash(i + c.xyx), hash(i + c.yyx),f.x),
        f.y),
		mix(mix(hash(i + c.xxy), hash(i + c.yxy),f.x),
        mix(hash(i + c.xyy), hash(i + c.yyy),f.x),
        f.y),
		f.z);
}

float fbm(vec3 p, vec3 fft, vec3 wav) {
	return .5000 * noise(1. * p, fft, wav)
       + .2500 * noise(2. * p, fft, wav)
	     + .1250 * noise(4. * p, fft, wav)
	     + .0625 * noise(8. * p, fft, wav);
}

vec4 fog() {
  /* vec3 fft = texture2D(tFogNoise, vec2(0.2 * vUv.x + 0.1 * sin(time), */
  /*                                      0.3 * vUv.y - 0.09 * cos(time))).rgb; */
  /* vec3 fft = texture2D(tFogNoise, vec2(0.98 * vUv.x + 0.05 * sin(time), */
  /*                                      0.90 * vUv.y - 0.09 * cos(time) */

  /* vec2 uv = vUv; */
  /* vec2 uv = vec2(vProjCoord.x * 0.2, vProjCoord.y * 0.15); */
  vec2 uv = vec2(vUv.x*0.2 + vProjCoord.x * 0.13,
                 vUv.y*0.2 - vProjCoord.y * 0.02);
  vec3 fft = texture2D(tFogNoise, vec2(0.08 * uv.x + 0.04 * sin(time),
                                       0.18 * uv.y - 0.03 * cos(time)
                                                   + 0.02 * tan(fract(time/3.2)))).rgb;
  uv = vUv;
  vec3 fft2 = texture2D(tFogNoise, vec2(0.16 * uv.x - 0.03 * sin(time),
                                        0.1 + sin(0.3*time+0.1) * 0.09 * uv.y + 0.03 * cos(time)
                                                    - 0.02 * fract(time/2.2))).rgb;
  fft += fft2;
  /* vec3 wav = texture2DProj(tFogNoise, vProjCoord).rgb; */
  vec3 wav = texture2DProj(tFogNoise, uv.xyxy).rgb;
	/* float t  = cos(fft.x * 20.0 / 3.14159265); */
	float t  = cos(fft.x * 2.2 / 3.14159265);
	float cost = cos(t);
	float sint = sin(t);

	/* vec2 uv = vUv; */
	vec2 vc = (2.0 * uv - 1.0) * vec2(uv.x / uv.y, 1.0);
	/* vec2 uv = fragCoord.xy / iResolution.xy; */
	/* vec2 vc = (2. * uv - 1.) * vec2(iResolution.x / iResolution.y, 1.); */

	vc = vec2(vc.x * cost - vc.y * sint,
            vc.y * cost + vc.x * sint);

	vec3 rd = normalize(vec3(0.1, vc.y, vc.x));
	/* vec3 rd = normalize(vec3(uv.y, vc.y, vc.x)); */
  float contrast = 1.8;
	vec3 c = contrast * vec3(fbm(rd, fft, wav)) * fft.xyz;
	c += hash(hash(uv.xyy) * uv.xyx * time) * 0.1;
	/* c += hash(hash(uv.xyx) * uv.xyy * time) * 0.1; */
	c *= 0.7 * smoothstep(length(uv * 0.24 - 0.12), 0.60, 0.9);

	return vec4(c, 1.0);
}


void main() { // wishlist: fix angle thing, moving fog noise texture, 
  //vec3 diffuse   = texture2D(tDiffuse, vUv).rgb;
  if(dimmer < 0.01) { // also: "black" cant exist as color - make transparent
    gl_FragColor	     = vec4(vec3(0.0), 0.0); //skip
  } else {
    float distanceFade = distance(vModelM.xyz, spotPosition) / furthest; // attenuation by distance
    float intensity	   = cap - clamp(distanceFade, 0.0, cap);            // only place a hard cap works is here

    vec3 normal	  = vec3(vNormal.x, vNormal.y, abs(vNormal.z));            // so normalized normal, abs z bc why?
    vec3 straight = vec3(0.0, 0.0, 1.0);
    /* float angleIntensity1	= pow(abs(dot(vNormal, vNormalPos)), anglePower); //this gets weird when facing */
    float angleIntensity2	= pow(abs(dot(normal, straight)), anglePower); //this gets weird when facing
    /* float angleIntensity3 = pow(abs(dot(vNormalPos, vActualNormal)), 6.0); //this gets weird when facing */
    /* float angleIntensity3 = pow(1.0 - abs(dot(vNormal, vActualNormal)), 3.0); //this gets weird when facing */
    /* float angleIntensity4 = (dot(vNormal, vActualNormal)); //this gets weird when facing */
    /* float angleIntensity = angleIntensity1*0.3 + angleIntensity2*0.7 + 0.05+angleIntensity3*0.2; */
    float angleIntensity = angleIntensity2;
    /* float angleIntensity = mix(angleIntensity2, angleIntensity2, angleIntensity3); */

    float radius      = 5.115;
    float ourDepth    = toLinearDepth(gl_FragCoord.z);
    float bufferDepth = toLinearDepth(texture2DProj(tDepth, vProjCoord).x); //correct
    /* float diff        = clamp(abs(ourDepth - bufferDepth) / radius, 0.0, 1.0); // 'how close to anything solid?' */
    // ^^ sometimes - is right, sometimes +, what gives??? makes sense to go 0 rather than abs in any case, no leaks.
    float diff          = clamp((bufferDepth - ourDepth) / radius, 0.0, 1.0); // 'how close to anything solid?'
    float fadeNearNear  = clamp(ourDepth / 3.0, 0.0, 1.0);     // avoid clipping cone when flying through. not sure if worth?
    intensity	         *= dimmer * fadeNearNear * diff * opacity * angleIntensity;
    /* float s = abs(sin(time)); */
    float ft = fract(time / 2.2);
    float s = 1.2 * (sin(time*0.4));
    float sf = cos(ft);

    vec4 noiseA = texture2D(tFogNoise, vec2(2.5*vUv.x - 0.3 * s + 0.25 * sf,
                                            3.9*vUv.y + 0.2 * sf));
    /* vec2 uv = vec2(0.2 * vUv.x - 0.1 * s, 0.2 * vUv.y); */
    vec2 uv = vec2(0.05 * vUv.x - 0.1 * tan(s), 0.1 * vUv.y);
    vec3 noiseB = texture2D(tFogNoise, vec2(uv.x, 1.0 - uv.y)).rgb;

    vec3 fog = fog().rgb;
    vec3 color = lightColor + 0.090*vec3(noiseB.rgb)
                            - 0.220*vec3(noiseA.rg*0.115, noiseB.b*0.23)
    /* vec3 color = lightColor + 0.010*vec3(noiseB.rg, noiseA.b) */
    /*                         - vec3(noiseA.rg*0.015, noiseB.b*0.06) */
                            - fog*0.25 * (0.3+clamp(abs(0.8 * sin(ft)), 0.2, 0.6));

    gl_FragColor	     = vec4(color, intensity - 0.022*noiseB);                         // set the final color
    /* gl_FragColor	     = vec4(lightColor, intensity);                         // set the final color */
    /* gl_FragColor	     = vec4(fog, intensity + 0.02*noise);                         // set the final color */
    /* gl_FragColor	     = vec4(vec3(diff), 0.8);                         // set the final color */
    /* gl_FragColor	     = vec4(vec3(diff*angleIntensity), 0.8);                         // set the final color */
  }
}
