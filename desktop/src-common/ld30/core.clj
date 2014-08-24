(ns ld30.core
  (:require [play-clj.core :refer :all]
            [play-clj.ui :refer :all]
            [play-clj.math :refer :all]))

(defn get-entity-at-point
  [entities point]
  (find-first (fn [{:keys [x y width height] :or {x 0 y 0 width 0 height 0} :as entity}]
                (rectangle! (rectangle x y width height) :contains (:x point) (:y point)))
              entities))

(defn get-entity-at-cursor
  [screen entities input-x input-y]
  (let [point (input->screen screen input-x input-y)]
    (get-entity-at-point entities point)))

(defn set-location-wants
  [entity]
  (let [wants (+ (rand-int 10) 1)]
    (assoc entity
           :wants wants)))

(defn update-location
  [loc]
  (let [els(for [el (:entities loc)]
            (case (:id el)
              :wants-label (doto el (label! :set-text (format "Wants %d centurion hats" (:wants loc))))
              :angry-label (doto el (label! :set-text (format "Angry! %d" (:anger loc))))
              el))]
    (merge loc (apply bundle els))))

(defn make-location
  [x y]
  (let [w 200
        h 40]
    (set-location-wants (assoc (bundle (assoc (shape :filled
                                              :set-color (color :red)
                                              :rect 0 0 w h)
                                              :id :background)
                                       (assoc (label "Makes: Centurion Hats" (color :white))
                                              :y (+ y 15)
                                              :id :makes-label)
                                       (assoc (label "Angry! 0" (color :white))
                                              :y (+ y 30)
                                              :id :angry-label)
                                       (assoc (label "Wants 4 Centurion Hats" (color :white))
                                              :id :wants-label))
                               :x x 
                               :y y 
                               :width w 
                               :id :temp
                               :height h 
                               :location? true
                               :anger 0
                               :center {:x (+ x (/ w 2)) :y (+ y (/ h 2))}))))


(defn make-link
  [e1 e2]
  (let [x1 (:x (:center e1))
        y1 (:y (:center e1))
        x2 (:x (:center e2))
        y2 (:y (:center e2))]
    (assoc (shape :line 
                  :line x1 y1 x2 y2
                  :set-color (color :cyan))
           :link? true
           :id :temp
           :points [{:x x1 :y y1}, {:x x2, :y y2}])))

(defn is-active-link
  [{:keys [x y width height] :as loc} link]
  (some (fn [point] (rectangle! (rectangle x y width height) :contains (:x point) (:y point))) (:points link)))

(defn get-linked-locs
  [loc entities]
  (let [locs (filter :location? entities)
        links (filter :link? entities)
        active-links (filter (fn [link] (is-active-link loc link)) links)
        active-locs (for [l locs
                      :let [total (count (filter (fn [link] (is-active-link l link)) active-links))]
                      :when (> total 0)]
                      l)]
    (filter #(not= (:center loc) (:center %)) active-locs)))

(defn connected-to-want 
  [loc entities]
  (let [link-count(count (get-linked-locs loc entities))]
    (> link-count 0))
  false)


(defn update-entity 
  [screen entities entity]
  (cond 
    (:location? entity)
    (let [connected (connected-to-want entity entities)]
      (if connected
        (update! screen :score (+ (:score screen) 1)))
      (let [entity(if connected
                    (assoc entity :wants (- (:wants entity) 1))
                    (assoc entity :anger (+ (:anger entity) 1)))]
        (update-location entity)
      ))
    (= (:id entity) :score)
    (doto entity (label! :set-text (format "Score: %d" (:score screen))))
    :else entity))

(defn pulse-entities
  [screen entities]
  (let [entities(for [e entities]
                  (update-entity screen entities e))]
    (if (every? #(>= (:anger %) 4) (filter :location? entities))
      (doall [(println "GAME OVER, MAN, GAME OVER"), (app! :exit)])
      entities
    )))

(defscreen main-screen
  :on-show
  (fn [screen entities]
    (update! screen 
             :score 0
             :renderer (stage))
    (add-timer! screen :pulse 1 1)
    (let [loc1 (make-location 40 35)
          loc2 (make-location 400 150)
          loc3 (make-location 250 500)
          loc4 (make-location 60 400)]
      [loc1
       loc2
       loc3
       loc4
       (make-link loc1 loc3)
       (make-link loc1 loc2)
       (assoc (label "Score: 0" (color :white))
            :id :score
            :x 15 :y 570)]))

  :on-key-down
  (fn [screen entities]
    (cond
      (= (:key screen) (key-code :a))
      (for [entity entities]
        (if (:location? entity)
        (update-location entity) entity))
      (= (:key screen) (key-code :q))
      (app! :exit)
      ))

  :on-touch-down
  (fn [screen entities]
    (println (let [e (get-entity-at-cursor screen entities (:input-x screen) (:input-y screen))]
               (if (and e (:location? e)) (get-linked-locs e entities))
               ))
    entities)

  :on-timer
  (fn [screen entities]
    (case (:id screen)
      :pulse (pulse-entities screen entities)
      entities))

  :on-render
  (fn [screen entities]
    (clear!)
    (render! screen entities)))

(defgame ld30
  :on-create
  (fn [this]
    (set-screen! this main-screen)))
