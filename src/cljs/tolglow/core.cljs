(ns tolglow.core
  (:require tolglow.cue))

(defn on-js-load []
  (enable-console-print!)
  (println "JS LOADED")
  #_(cue/init)) ;set up a fn for the init stuff...
