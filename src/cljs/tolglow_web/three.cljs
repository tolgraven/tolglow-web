(ns tolglow-web.three
  (:require ; ["react-dom/server" :as ReactDOMServer :refer [renderToString]]
            [three :as three]
            ["three/examples/jsm/controls/OrbitControls"         :as orbit]
            ["three/examples/jsm/loaders/ColladaLoader"          :as collada]
            ["three/examples/jsm/postprocessing/EffectComposer"  :as composer]
            ["three/examples/jsm/postprocessing/UnrealBloomPass" :as ubpass]
            ; ["three/examples/jsm/postprocessing/FilmPass"        :as fpass]
            ["three/examples/jsm/postprocessing/RenderPass"      :as rpass]
            ; BELOW BROKEN?? bugs out when import
            ; ["three/examples/jsm/postprocessing/BokehPass"       :as bokeh]
            ; ["three/examples/jsm/postprocessing/ShaderPass"      :as shader]
            ; [thi.ng.glsl.core :as glsl :include-macros true]
            [cljsjs.tween]  ;animate by setting target and interpolating to it. do fix formal similar built into glow.
            [tolglow-web.events :as ev :refer [log]]
            [tolglow-web.util :as util]
            [applied-science.js-interop :as j]
            [reagent.core :as r]
            [clojure.core.async :refer [pub sub chan go go-loop >! <! timeout close! unsub unsub-all sliding-buffer]]
            [re-frame.core :as rf]))
;; ************************************************************
;; I mean 97% certain geom and his gl will be best to work with...
;; so skip all this crap bleh it randomly just broke now anyways
;; ************************************************************
(defonce ^{:doc "Because we also live-reload shaders, they are stored here."}
  globals
 (atom
  {:shader
   {:cone
    {:vert "void main() { gl_Position	 = projectionMatrix * modelViewMatrix * vec4(position, 1.0); }"
     :frag "void main() { gl_FragColor = vec4(vec3(0.6, 0.2, 0.2), 0.3); }"}
    :glow
    {:vert "uniform vec3 viewVector;
            uniform float powTo;
            varying float intensity;
            void main() {
              gl_Position = projectionMatrix * modelViewMatrix * vec4(position, 1.0);
              vec3 actual_normal = vec3(modelMatrix * vec4(normal, 0.0));
              //intensity = pow(dot(normalize(cameraPosition - position), actual_normal), powTo);
              intensity = pow(dot(normalize(viewVector), actual_normal), powTo);
            }"
     :frag "varying float intensity;
            uniform vec3 lightColor;
            uniform float dimmer;
            void main() {
              vec3 color   = lightColor * (1.0 - intensity);
              gl_FragColor = vec4(lightColor, intensity * dimmer);
            }"}}}))

(defonce ^{:doc "We shan't store our own shit in app-db (our updates aint in the VDOM) so"}
 spotlights (atom {}))
(def active-lights "Keep track of active (in-scene) lights. Need a system for swapping lights without .add/.remove"
 (atom #{}))
(def fixture-radius 0.2)


(defn get-renderer "Init new renderer. We should maybe store these so avoids recreating..." [& [w h]]
 (doto (three/WebGLRenderer.)
  (.setPixelRatio (.-devicePixelRatio js/window))
  ; (.setClearColor 0x202020 0.5)
  (.setSize 600 450) ;or skip setting at all here, but then need to make 100% sure gets set later?
  (j/assoc! ;:antialias     true
            ; :physicallyCorrectLights true
            :gammaInput    true
            :gammaOutput   true
            :alpha         true) ;for transparent bg
  ; :toneMapping   three/ReinhardToneMapping
  ; :toneMappingExposure (Math/pow 1.4 5.0)
  (j/assoc-in! [:shadowMap :enabled] true)
  (j/assoc-in! [:shadowMap :type]    three/PCFSoftShadowMap)))

(defn get-composer [[w h] renderer scene camera]
 (doto (composer/EffectComposer. renderer)
  (.addPass (rpass/RenderPass. scene camera))
  ; (.addPass (doto (three/ShaderPass. three/AdditiveBlendingShader )
  ;            (j/assoc-in! [:uniforms :tAdd :value] (.-texture occlusion-target))));
  (.addPass (let [res (three/Vector2. w h) ; viewport resolution
                  [strength radius threshold] [0.10 0.20 0.60]]
             (j/assoc! (ubpass/UnrealBloomPass. res strength radius threshold)
                       :renderToScreen true)))
  ; (.addPass (j/assoc! (bokeh/BokehPass.
  ;                      scene camera
  ;                      (clj->js {:maxBlur 1.0
  ;                                :width w :height h
  ;                                :focus 1 :aperture 0.025}))
  ;                     :renderToScreen true))
  ; (.addPass (let [[noise [scan-pwr scan-count] grayscale]
  ;                 [0.27 [0.00 0] false]]
  ;            (j/assoc! (fpass/FilmPass. noise scan-pwr scan-count grayscale)
  ;                     :renderToScreen true)))
  ))


(defn get-scene []
 (let [[color near far :as fog] [0x131115  3.8 120]]
 (doto (three/Scene.)
  (j/assoc! :fog (three/Fog. color near far)
            :background (three/Color. 0x181816)))))


(defn get-camera [& {:keys [aspect fov position lookat]
                     :or   {aspect 1.333 fov 35 position [0 3 -5] lookat [0 0 0]}}]
   (let [[x y z]    position
         [xl yl zl] lookat
         [near far] [0.1 200]]              ;; this ratio has big impact on depth texture resolution
    (doto (three/PerspectiveCamera.  fov aspect near far)
     (j/update! :position j/call :set x y z)
     (.lookAt (three/Vector3. xl yl zl)))))

(defn get-controls [camera renderer];this is stealing arrow keys even when not selected...
  (doto (orbit/OrbitControls. camera (.-domElement renderer)) ; suddenly complaining "not a constructor" but three ver is pinned and shit??
    (j/update! :target j/call :set 0 2 0)))


(defn depth-texture-target [[w h]]
 (doto (three/WebGLRenderTarget. w h)
  (j/assoc-in! [:texture :format]    three/RGBFormat)
  ; (j/assoc-in! [:texture :minFilter] three/NearestFilter) ;default (and req for depth texture)
  (j/assoc-in! [:texture :generateMipmaps] false) ;req for depth texture
  (j/assoc! :stencilBuffer false
            :depthBuffer   true
            :depthTexture  (doto (three/DepthTexture.)
                            (j/assoc! :type three/UnsignedShortType)))))


; (def texture (.load (three/TextureLoader.) "PerlinNoise2d.png" ; "3d-models/graynoise.png"
(def texture (.load (three/TextureLoader.)  "3d-models/graynoise.png" ;  "3d-models/perlin.png"
                    (fn [tex]
                     (doto tex
                      (j/assoc! :wrapS three/RepeatWrapping
                                :wrapT three/mirroredRepeatWrapping))
                     (swap! globals assoc :fog-texture tex))))

(defn volu-spot-material [& {:keys [camera d-target]}] ;or just fragment-shader etc (dont make new obj)
 (let [vertex-shader   (get-in @globals [:shader :cone :vert]) ;or just 0 i guess uh
       fragment-shader (get-in @globals [:shader :cone :frag]) ;temp for diag
       ; fog-texture     (.load (three/TextureLoader.) "3d-models/graynoise.png") ;can also give callback, maybe better dunno
       uniforms
       {:near          {:value (.-near camera)}
        :far           {:value (.-far  camera)}
        :tDiffuse      {:value (.-texture      d-target)}
        :tDepth        {:value (.-depthTexture d-target)}
        ; :tFogNoise     {:type "t"  :value (.load (three/TextureLoader.) "img.jpg")}
        :tFogNoise     {:type "t"  :value (:fog-texture @globals)}
        :furthest      {:type "f"  :value 20.0}
        :anglePower    {:type "f"  :value 3.5} ;looks nicer high but until fix weirdo camera angle stuff gotta keep low
        :spotPosition  {:type "v3" :value (three/Vector3. 0 0 0)}
        :lightColor    {:type "c"  :value (three/Color. "white")}
        :time          {:type "f"  :value (- (.now js/Date.) 1550000000000)}
        :opacity       {:type "f"  :value 0.56}
        :cap           {:type "f"  :value 0.72} ;opacity 0.65 cap 0.60 quite nice but gotta fix so can see shit even when pointing my dir...
        :dimmer        {:type "f"  :value 0.0 #_1.00}} ;oh right default 0.0 much better...
       material
       (three/ShaderMaterial.
        (clj->js
         {:uniforms        uniforms
          :vertexShader    vertex-shader
          :fragmentShader  fragment-shader
          ; :side		          three/DoubleSide
          ; :blending	       three/AdditiveBlending ;auto get rid of black. but hmm...
          :transparent     true
          :depthWrite      false}))] ;this should ensure were not incl in depth texture right??
  material))

;welp clj only apparently...
; (glsl/defglsl-file frag [] "resources/public/glsl/frag.glsl")

(defn update-shader-async [ch material] ;vertexShader, fragmentShader... whatever else goes into ShaderMaterial.
 (log "Create shader updater")
 (go-loop []
          (when-let [{:keys [kind text]} (<! ch)]
           (log "Run update shader...")
           (swap! globals assoc-in [:shader :cone kind] text)
           (doseq [[id {:keys [cone]}] @spotlights] ;uuuugh i mean fast whatever just tots not elegant
            (j/assoc! (:material cone)
                      (if (= kind :vert) :vertexShader :fragmentShader) text
                      :needsUpdate true)
            (j/assoc-in! (:material cone) [:uniforms :tFogNoise :value] (:fog-texture @globals)))
           (recur)))
 (log "Shutdown shader updater..."))

(declare load-model)


(def spot-cone-geom ;; at least this can be reused! except will likely want different sizes/lengths, def different angles...
 (let [[start-radius end-radius length] [fixture-radius 3.6 18]]     ;;radius at thinnest, widest, length of cone
  (doto (three/CylinderGeometry.
         start-radius, end-radius, length, 64, 20, true)  ;;num polygons or w/e + true for open at cone end
   (.applyMatrix (.makeTranslation (three/Matrix4.)  0 (/ length -2) 0))  ;; move half length so we're rotating from base and not middle
   (.applyMatrix (.makeRotationX   (three/Matrix4.)  (/ js/Math.PI -2)))))) ;; rotate -90deg to bring us inline with light (XXX understanding why would be nice)


(defn create-spot-cone "Should just run updater instead of setting values manually?"
 [material light]
  (let [geometry spot-cone-geom ;initially pivot point is at center, move it so is correct.
        material (doto material ;; or skip setting these now as will be done at
                  (j/assoc-in!  [:uniforms :spotPosition :value] (.-position light)))
        mesh (doto (three/Mesh. geometry material)
              (j/assoc! :castShadow    false
                        :receiveShadow false))]
    #_(j/assoc! light
              :shadowCameraNear   0.01   :shadowCameraFar        15
              :shadowCameraFov      45   :shadowCameraVisible  true
              :shadowCameraLeft     -8   :shadowCameraRight       8
              :shadowCameraTop       8   :shadowCameraBottom     -8
              :shadowBias	         0.0   :shadowDarkness	       0.5
              :shadowMapWidth	    1024   :shadowMapHeight	     1024)
    {:material material :mesh mesh}))


(defn create-spot-light "Actual light-source/emitter" [color]
  (j/assoc! (three/SpotLight.) ;
            :color       color ;
            :intensity   3     ;use intensity (fraction) as dimmer?? else just opacity or
            :castShadow  true  ; maybe should try having lots of light but skipping shadows?
            :angle       (/ js/Math.PI 6)
            :penumbra    0.78  ;0-1. Fraction of light softened
            :decay       1.8   ;around 2 natural
            ; :power     intensity * Pi when physically correct...
            :distance    60))  ;max distance. default 0 (no limit)

(defn create-spot-model "Fixture 3d model" [{:keys [x y z] :as data}]
  (three/SphereGeometry. x y z))


(defn glow-material
 [light]
 (let [vertex-shader   (get-in @globals [:shader :glow :vert]) ;or just 0 i guess uh
       fragment-shader (get-in @globals [:shader :glow :frag]) ;temp for diag
       uniforms
       {:viewVector    {:value (.-position light)} ;matrixWorld for own mesh would be reasonable and not need uniform or?
        :lightColor    {:type "c"  :value (three/Color. "black")}
        :dimmer        {:type "f"  :value 0.0}
        :powTo         {:type "f"  :value 4.0}} ;oh right default 0.0 much better...
       material
       (three/ShaderMaterial.
        (clj->js
         {:uniforms        uniforms
          :vertexShader    vertex-shader
          :fragmentShader  fragment-shader
          :blending	       three/AdditiveBlending ;try to get rid of the black corners bleh!
          :transparent     true
          :depthWrite      false}))]
  material))

(defn create-spot-glow "Fixture glow plate"
 [light]
 (let [rad      (* 1.1 fixture-radius)
       geometry (doto (three/SphereGeometry. rad 18 18)
                 #_(.applyMatrix (.makeTranslation (three/Matrix4.)  0 (/ rad 2) 0))
                 #_(.applyMatrix (.makeRotationX   (three/Matrix4.)  (/ js/Math.PI -2))))
       material (glow-material light)
       mesh     (doto (three/Mesh. geometry material)
                 (j/assoc! :castShadow    false
                           :receiveShadow false))]
  {:material material :mesh mesh}))

(defn create-spot-plate "Fixture plate"
 [light]
 (let [rad      (* 0.38 fixture-radius)
       geometry (doto (three/SphereGeometry. rad 12 12)
                 ; #_(.applyMatrix (.makeTranslation (three/Matrix4.)  0 (/ 0.1 2) 0))
                 #_(.applyMatrix (.makeTranslation (three/Matrix4.)  0 (/ rad 2) 0))
                 #_(.applyMatrix (.makeRotationX   (three/Matrix4.)  (/ js/Math.PI -2))))
       material (doto (three/MeshLambertMaterial.)
                 (j/update! :color j/call :set 0x151510))
       mesh     (doto (three/Mesh. geometry material)
                 (j/assoc! :castShadow    false
                           :receiveShadow true))]
  {:material material :mesh mesh}))


(defn create-spot-group "Common group for cone, glow, model, so can control position and rotation at once?"
 [light & objects]
 (let [[cone glow plate] objects
       group (three/Group.)]
  (doto group
   (.add (:mesh cone))
   (.add (:mesh glow))
   (.add (:mesh plate))
   (.lookAt (three/Vector3. 0 0 0))
   (j/update! :position j/call :copy (.-position light)))))


(defn toggle-spotlight!
 [state scene {:as fixture-data :keys [light helper group cone glow]}]
 (let [show-helper false
       use-light false] ;should calculate based on lots of stuff. what kinda fixture, strength, amount already lit, yada. first throttle shadows, then quit light, keeping cones...
  (case state
   :on (when-not false ;(some #{(:id fixture)} @active-lights)
        ; (swap! active-lights conj (:id fixture))
        (doto scene
         (.add (.-target light)) ;; need to add light's .-target to scene to use it as reference (wont auto-update otherwise)
         (.add group))
        (when show-helper (.add scene helper))
        (when use-light   (.add scene light)))        ;; debug - lines showing where light is pointed
   ;; add light itself to scene - will have to limit for performance...

   :off (when true ;(some #{(:id fixture)} @active-lights)
         ; (swap! active-lights disj (:id fixture))
         (doto scene
          (.remove (.-target light))
          (.remove group))
         (when show-helper (.remove scene helper))
         (when use-light   (.remove scene light))))))

(defn toggle-just-light!
 [state scene {:as data :keys [light fixture]}]
 (case state
  :on (when (and (> 6 (count @active-lights))
                 (not (some #{(:id fixture)} @active-lights)))
        (swap! active-lights conj (:id fixture))
        (.add scene light))        ;; debug - lines showing where light is pointed
   :off nil)) ;cant be removing from scene, stutters like madd


(defn init-spotlights-clean! [s scene camera d-target fixtures]
 (let [cone-material (volu-spot-material :camera camera :d-target d-target)]
  (doseq [[id {:keys [id x y z] :as fixture}] fixtures
          :let [cone-material (volu-spot-material :camera camera :d-target d-target)]] ;FRIENDLY REMINDER THAT CLONE MAKES DEPTH TEXTURE STOP WORKING
   (let [light  (doto (create-spot-light (three/Color. 0.0 0.0 0.0)) ;p useless creating light itself when still black? hmm
                 (j/update! :position j/call :set x y z)) ;XXX get/setup initial rotation of fixture!!
         cone   (create-spot-cone cone-material light)
         glow   (create-spot-glow light) ;prob just clone these...
         plate  (create-spot-plate light)
         group  (create-spot-group light cone glow plate)
         ; model  (load-model group :moving-head "/3d-models/moving_head.dae" [0 0 0])
         helper (three/SpotLightHelper. light)
         data {:fixture fixture :light light :helper helper
               :group group :glow glow :cone cone :plate plate}]
    (toggle-spotlight! :on scene data)
    (swap! spotlights assoc id data)))))



(defn box []
  (let [mat (three/MeshPhongMaterial. #js {:color 0x8982a8})
        geo (three/BoxBufferGeometry. 1 1 0.5)
        mesh (doto (three/Mesh. geo mat)
               (j/assoc! :castShadow    true
                         :receiveShadow true)
               (j/update! :position j/call :set 0 1 0))]
    {:material mat :geometry geo :mesh mesh}))

; (defn create-mesh [color {:as geom :keys [w h d]} mesh-opts [x y z :as position]]
;   (let [mat (three/MeshPhongMaterial. #js {:color color})
;         geo (three/BoxBufferGeometry. 1 1 0.5)
;         mesh (doto (three/Mesh. geo mat)
;                (apply j/assoc! mesh-opts)
;                (j/update! :position j/call :set x y z))]
;     {:mat mat :geo geo :mesh mesh}))

(defn create-wall [side]
  (let [geometry  (three/BoxGeometry. 20, 20.0, 5, 5, 10, 5);
        material	(three/MeshPhongMaterial.
                   #js {:color (three/Color. 0x304045)})
        mesh	    (doto (three/Mesh. geometry material)
                    (j/assoc! :receiveShadow	true
                              :castShadow		  true
                              :rotateX       (/ js/Math.PI 2))
                    (j/update! :position j/call :set
                               0 (/ (-> geometry .-parameters .-height) -2) -1))]
    mesh))

; (def loading-manager
;  (three/LoadingManager. (fn [] :wtfthisfor)))
(defn load-model [parent id file-path [x y z]] ;scene or group or w/e
 (if-let [model (get-in @globals [:models id])]
  (.add parent (.-scene model)) ;already loaded
  (doto (collada/ColladaLoader.) ;should reuse i guess...
   (.load file-path
          (fn [model]
           (log :debug "Loaded model" id)
           (let [loaded (.-scene model)
                 local (three/Object3D.)]
            (doto local ; (j/update! :position j/call :set 20 0 -20)
             )
            (doto loaded
             (j/assoc! :receiveShadow	true :castShadow		  true)
             (j/update! :position j/call :set x y z)
             #_(j/update! :scale j/call :set 0.5 0.5 0.5)) ;gets super fucked hmm
            ; (swap! s assoc-in [:models id] model #_loaded) ;hmmm get stackoverflow trying to access from repl later heh
            (.add parent loaded)
            (swap! globals assoc-in [:models id] model))))))) ;could also pass a ch and async... cleaner?

; (defn flat-mat [target proj-cam]
;  (let [cam (three/OrthographicCamera. -1 1 1, -1 0 1)
;        scene (three/Scene.)
;        mat (volu-spot-material :camera proj-cam :d-target target)
;        quad (three/Mesh. (three/PlaneBufferGeometry. 2 2) mat)]
;   (.add scene quad)
;   {:material mat :mesh quad :scene scene :camera cam}))


(defn create-fogger []) ;realized maybe small particles best for catching beam but still passing through?
(defn create-background-fog [])


(defn load-physical! [s] ;whoops change so its all going to s...
 (let [wall (create-wall 0)
       box (:mesh (box))]
  (load-model (:scene @s) #_s :warehouse "/3d-models/WAREHOUSE/model.dae" [-40 -18 8])
  (load-model (:scene @s) #_s :truss     "/3d-models/truss/model.dae"     [1    0 0.5])
  (load-model (:scene @s) #_s :pallet    "/3d-models/pallet.dae"          [0   0  0])
  {:wall wall :box box}))


(defn init-general-lights! [scene]
  (let [direcho (let [[color opacity] [0x302015 0.13]]
                 (doto (three/DirectionalLight. color opacity)
                  (j/assoc! :castShadow true)))
        ambient (let [color 0x121417]
                 (three/AmbientLight. color 0.12))]
   (doto scene
    (.add ambient)
    (.add direcho))))



(defn create-tween [target m-fn ms-f & [easing]]
 (doto (js/TWEEN.Tween. target)
  (.to (clj->js (m-fn))
       (ms-f))
  (.easing (or easing js/TWEEN.Easing.Quadratic.Out))
  (.start)))

(defn tween [light]
  (create-tween light
                #(do {:angle    (+ 0.2 (* 0.75 (rand)))
                      :penumbra (+ 0.2 (* 0.70 (rand)))})
                #(+ 2000 (* 5000 (rand))))
  (create-tween (.-position (.-target light))
                #(do {:x (- (* (rand)   8)  5)
                      :y (+ (* (rand) -10) -5)
                      :z (- (* (rand)   9)  5)})
                #(+ 2000 (* 3000 (rand)))))

(defn animate []
  (doseq [[id {:keys [light]}] @spotlights]
    #_(tween light)))

(defn update-uniform [material k value]
 (j/assoc-in! material [:uniforms k :value] value))


(defn update-each-frame [camera scene spotlights started-at]
  ; (.update js/TWEEN)
  (let [elapsed-all (- (.now js/Date.) started-at)]
   (doseq [[id {:keys [light helper glow cone] :as data}] spotlights]
    (when helper (.update helper))
    (when cone
     (let [now (/ (.now js/Date.) 156000000)]
      ; (update-uniform (:material cone) :time now)))
      ; (update-uniform (:material cone) :time (Math/sin (.now js/Date.)))))
      ; (update-uniform (:material cone) :time elapsed-all)))
      (update-uniform (:material cone) :time (/ elapsed-all 10000 #_1000))))
    (when glow
     (let [;glow-pos (three/Vector3.)
           ; _ (.getWorldPosition (:mesh glow) glow-pos)
           viewV (doto (three/Vector3.)
                  (.subVectors (.-position camera) ;would only need update on camera movement. also couldn't do in-shader??
                               (.-position light)))]
      (update-uniform (:material glow) :viewVector viewV))))
   elapsed-all))

(defn fixture-updater "Async update. Take each updated value and update :fixtures, but also uniforms etc depending on content"
 ; also should do away with (:fixtures @spotlight) and just use app-db?
 ; and just custom sub to transform eg color format?
 ; ^ HELL no other way round. app-db _must_ be clean from stuff (or rather, updates)
 ; not pertinent to VDOM. Period.
 [s def-atom ch] ;per head, or per value? hmm. mainly, how handle fact dont know whats incoming
 (log "Create fixture updater")
 (go-loop [] ;thing now is we're not updating :fixtures so cant rely on it. but initial state + incoming + app-db plenty heh
          (when-let [{:keys [id color lookat dimmer strobe] :as fixture} (<! ch)] ;tho guess no reason to actually close this ever?
           (when-some [{:keys [fixture light group cone glow plate] :as data} (get @spotlights id)]

            (when dimmer
             (j/assoc! light :intensity (* 2 dimmer))
             (update-uniform (:material cone) :dimmer dimmer)
             (update-uniform (:material glow) :dimmer dimmer)
             (j/assoc! (:material plate) :emissiveIntensity dimmer)
             (let [dimmed-color (doto (.clone (.-color light))
                                 (.multiplyScalar dimmer))]
              (j/update! (:material plate) :color j/call :copy dimmed-color))
             (let [state (if (pos? dimmer) :on :off)]
              (toggle-just-light! state (:scene @s) data)))

            (when color
             (let [[h s l] @color] ;tho, well, not if getting partial values
              ; can break if Color. gets corrupted. try else assoc instead?
              (try (j/update! light  :color j/call :setHSL h s l)
                   (catch js/TypeError e
                    (j/assoc! light :color (doto (three/Color.) (.setHSL h s l)))))
              (j/update! (:material plate) :emissive j/call :copy (.-color light))
              (update-uniform (:material glow) :lightColor (.-color light))
              (update-uniform (:material cone) :lightColor (.-color light))) ;also dont forget to call whatever thing ends up toggling lights eventually

             (when-let [[x y z] lookat]
              (j/update-in! light [:target :position] j/call :set x y z)
              (.lookAt group (-> light .-target .-position))))) ;whoops dont shut down unless passed false

           (recur)))
 (log "Shutdown fixture updater"))



(defn window-size "Get size of browser window" []
 (map #(j/get js/window %) [:innerWidth :innerHeight]))

(defn render-depth [s] ;this doesn't seem to be taking anymore?
 (let [{:keys [camera depth renderer scene]} @s]
  (.setRenderTarget renderer (:target depth))
  (.render renderer scene camera) ;render depth texture. I was wrong thinking should (even can) have sep scene have sep scenes. adding to one removes from other, so...
  ; (.render (-> @s :post :scene) (-> @s :post :camera)))
  ))


; split visualizer from generic, and amap from three...
; so can easy play around with hot reload
; def want shader as a render target...  esp with live reload stuff (and supporting params etc)

(defn three-comp "React component holding Three.js visualizer renderer.
                  Takes desired canvas width (caps at window width), aspect ratio, and fixture data.
                  SHOULD: not be an ugly monster. Dunno how that would be achieved hah."
 [w ratio]
 (log "Creating three component")
 (let [s (atom {:req-size [w (/ w ratio)]})
       _ (def s s)
       real-size (r/atom nil)] ;ratom so get cb

       (letfn
        [(update-real-size! []
          (let [[w h] (apply map min (map @s [:req-size :max-size]))
                new-size [w (/ w ratio)]]
           (when-not (= @real-size new-size) ;only update when changed
            (reset! real-size new-size))))

         (on-size [& _]
          (swap! s assoc :max-size (window-size))
          (update-real-size!)
          (j/apply (:renderer @s) :setSize (clj->js @real-size))
          (j/apply (:composer @s) :setSize (clj->js @real-size))
          (let [[w h] (map #(* % (.getPixelRatio (:renderer @s))) @real-size)]
           (j/assoc! (-> @s :depth :target) :width w :height h))
          (j/assoc! (:camera @s) :aspect (apply / @real-size))
          (.updateProjectionMatrix (:camera @s)))

         (start-render []
          (let [delta (.getDelta (:clock @s))]
           (.update (:controls @s)) ; required if controls.enableDamping or controls.autoRotate are set to true
           ; XXX best way to throttle when little or nothing happening?  but also like no lights on just wait for something to happen...

           (update-each-frame (:camera @s) (:scene @s) @(:spotlights @s) (:started-at @s))
           (render-depth s) ;try sep what's specific to viz
           (try (.render (:composer @s)) (catch js/TypeError t)) ;does weird things in beginning. shader not ready?
           ; (.setRenderTarget (:renderer @s) nil) (.render (:renderer @s) (:scene @s) (:camera @s))

           (swap! s assoc :anim-id (.requestAnimationFrame js/window start-render)))) ;some ex has theirs at top of render?


         (playback! [state] ;play pause etc fix
          (when-not (= state (:running @s)) ;skip duplicate requests
           (let [fixture-ch @(rf/subscribe [:get :ch :fixtures]) ; still cant decide whether bril or crap. def crap since now subbing here. but something about it is smart i just know it  #_(chan (sliding-buffer 100))
                 ;; maybe we just pass down ch to comp. still, stashing chan in db is just kool.
                 shader-ch  @(rf/subscribe [:get :ch :shaders]) #_(chan (sliding-buffer 4))] ;should be enough? got a 1024 msg even with this (!??)
            (case state
            :play (do (log "Start playback!")
                   ; (.setInterval      js/window animate 5000) ;run animation starter every 5000ms
                   (start-render)

                   (update-shader-async shader-ch (:cone-material @s))
                   (fixture-updater s spotlights fixture-ch) ;if fixture-state only starts later and creates ch, how handle?
                   (rf/dispatch [:subscribe :start :fixture-state]) ;well more, ensure is running, but...
                   (rf/dispatch [:subscribe :start :live-shaders]))

            :pause (do (log "Pause playback!")
                    (rf/dispatch-sync [:subscribe :stop :fixture-state]) ;dev-only issue but can lose order and these run *after* start...
                    (rf/dispatch-sync [:subscribe :stop :live-shaders])  ;hence sync for now.
                    (go (>! fixture-ch false)) ;better now?
                    (go (>! shader-ch  false))

                    (doto js/window ;^either this or try to check for existing chan hmm
                     (.clearInterval        animate)          ;stop calling animate
                     (.cancelAnimationFrame (:anim-id @s)))))
           (swap! s assoc :running state))))

         (on-mount [this] (log "Mounting three component")
          (let []
           (swap! s assoc ;change to let then swap heh...
                  :renderer (get-renderer)
                  :camera   (get-camera :position [0 5 -6])
                  :scene    (get-scene))
           (swap! s assoc
                  :controls  (get-controls (:camera @s) (:renderer @s))
                  :clock     (three/Clock.)
                  :started-at (.now js/Date.)

                  :depth {:target   (depth-texture-target [600 450])} ;w/h will be resized. needs to be created before on-size...
                  :composer  (get-composer [600 450] (:renderer @s) (:scene @s) (:camera @s))) ;then can use that when passing s to loader cb closure hmm
           (swap! s assoc
                  :models    (load-physical! s)
                  :spotlights spotlights)

           (.addEventListener js/window "resize" on-size)
           (on-size) ;run once manually to init window size...

           ; (swap! s assoc :post (flat-mat (:depth-target @s) (:camera @s)))
           (doseq [obj (vals (:models @s))] ;add basic meshes to main scene
            (.add (:scene @s) obj)) ;will only contain the meshes...

           (init-general-lights! (:scene @s))
           (init-spotlights-clean! s (:scene @s) (:camera @s)
                                   (-> @s :depth :target)
                                   @(rf/subscribe [:get :fixtures]))

           (.appendChild (:div-ref @s) (.-domElement (:renderer @s)))  ;; Put canvas as child of main div. saw something .replaceChild?
           (start-observer playback!) ;auto pause/play depending on visibility
           (log "Done mount, start rendering...")
           (playback! :play)))


         (on-unmount [this] (log "Unmounting three component")
          (playback! :pause)
          (doto js/window
           (.removeEventListener  "resize" on-size))
          (.dispose (:renderer @s)) ;dunno if needed but. also camera, scene?
          (.dispose (:controls @s))
          (.dispose (:scene @s)))

         (r-render [w ratio]  (log "Render div THREE")
          (swap! s assoc :req-size [w (/ w ratio)])
          (let [[w h] (update-real-size!)] ;dont need to deref real-size as fn does that
           [:div.three {:style {:width w :height h :margin "auto"}
                        :ref #(swap! s assoc :div-ref %)}]))

         (start-observer [playback-fn] ;what's with the weird scrolling bug?
          (doto (js/IntersectionObserver. ;prob needs tearing down for reload or?
                 (fn [entries]
                  (let [pos (.-intersectionRatio (first entries))]
                   (playback-fn (if (zero? pos) ;turns 0 both above and below!
                                 :pause :play)))))
           (.observe (:div-ref @s))))]

  (r/create-class
   {:display-name "three"
    :component-did-mount    on-mount
    :component-will-unmount on-unmount
    :reagent-render         r-render}))))
