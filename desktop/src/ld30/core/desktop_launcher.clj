(ns ld30.core.desktop-launcher
  (:require [ld30.core :refer :all])
  (:import [com.badlogic.gdx.backends.lwjgl LwjglApplication]
           [org.lwjgl.input Keyboard])
  (:gen-class))

(defn -main
  []
  (LwjglApplication. ld30 "ld30" 800 600)
  (Keyboard/enableRepeatEvents true))
