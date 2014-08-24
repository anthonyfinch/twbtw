(ns ld30.core
  (:require [play-clj.core :refer :all]
            [play-clj.ui :refer :all]
            [play-clj.math :refer :all]))

(def products-list
  [:books
   :costumes
   :knickknacks
   :cakes
   :bovril
   :hammocks
   :pie
   :kites
   :fulfillment
   :sourness
   :ennui
   ])

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
  (assoc entity
        :makes (rand-nth products-list)))

(defn set-location-makes
  [entity]
  (let [wants (+ (rand-int 10) 1)
        wants-item (rand-nth products-list)]
    (assoc entity
           :wants wants
           :wants-item wants-item)))

(defn update-location
  [loc]
  (let [loc(if (< (:wants loc) 1)
             (set-location-makes loc)
             loc)
        els(for [el (:entities loc)]
            (case (:id el)
              :makes-label (doto el (label! :set-text (format "Makes %s" (name (:makes loc)))))
              :wants-label (doto el (label! :set-text (format "Wants %d %s" (:wants loc) (name (:wants-item loc)))))
              :angry-label (doto el (label! :set-text (format "Angry! %d" (:anger loc))))
              el))]
    (merge loc (apply bundle els))))

(defn make-location
  [x y]
  (let [w 200
        h 40]
    (update-location (set-location-makes (set-location-wants (assoc (bundle (assoc (shape :filled
                                              :set-color (color :red)
                                              :rect 0 0 w h)
                                              :id :background)
                                       (assoc (label "" (color :white))
                                              :y (+ y 15)
                                              :id :makes-label)
                                       (assoc (label "" (color :white))
                                              :y (+ y 30)
                                              :id :angry-label)
                                       (assoc (label "" (color :white))
                                              :id :wants-label))
                               :x x 
                               :y y 
                               :width w 
                               :id :temp
                               :height h 
                               :location? true
                               :anger 0
                               :center {:x (+ x (/ w 2)) :y (+ y (/ h 2))}))))))


(defn make-link
  [e1 e2]
  (println e1 e1)
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
  (let [link-count(count (filter #(= (:makes %) (:wants-item loc)) (get-linked-locs loc entities)))]
    (> link-count 0)))


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
    (if (every? #(>= (:anger %) 10) (filter :location? entities))
      (doall [(println "GAME OVER, MAN, GAME OVER"), (app! :exit)])
      entities
    )))

(defn make-active-link
  [screen entities loc]
  (println "placing first point")
  (update! screen :active-link-point (:center loc))
  entities)

(defn add-link
  [screen entities loc active-link-point]
  (println "placing second point")
  (update! screen :active-link-point false)
  (conj entities (make-link loc (get-entity-at-point entities active-link-point)))) 

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
    (let [e (get-entity-at-cursor screen entities (:input-x screen) (:input-y screen))
          active-link-point(:active-link-point screen)]
               (if (and e (:location? e)) 
                 (cond 
                   (and active-link-point (not= active-link-point (:center e))) 
                   (add-link screen entities e active-link-point)
                   :else
                   (make-active-link screen entities e))
                 entities)))

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
