(ns ld30.core.desktop-launcher
  (:require [ld30.core :refer :all])
  (:import [com.badlogic.gdx.backends.lwjgl LwjglApplication]
           [org.lwjgl.input Keyboard])
  (:gen-class))

(defn -main
  []
  (LwjglApplication. ld30 "ld30" 1024 768)
  (Keyboard/enableRepeatEvents true))
