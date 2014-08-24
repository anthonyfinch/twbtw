(ns ld30.core
  (:require [play-clj.core :refer :all]
            [play-clj.ui :refer :all]
            [play-clj.g2d :refer :all]
            [play-clj.math :refer :all]))

(declare ld30 main-screen end-screen)

(def ^:const anger-limit 25)
(def ^:const screen-width 1024)
(def ^:const screen-height 768)
(def ^:const loc-width 50)
(def ^:const loc-height 50)
(def ^:const circ-radius 10)

(def ^:const products-list
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

(defn abs [n] (max n (- n)))

(defn get-entity-in-box
  [entities rect]
  (find-first (fn [{:keys [box-x box-y width height] :or {box-x 0 box-y 0 width 0 height 0} :as entity}]
                (rectangle! (rectangle box-x box-y width height) :overlaps rect))
              (filter #(not= (:id %) :background-tile) entities)))

(defn get-entity-at-point
  [entities point]
  (find-first (fn [{:keys [box-x box-y width height] :or {box-x 0 box-y 0 width 0 height 0} :as entity}]
                (rectangle! (rectangle box-x box-y width height) :contains (:x point) (:y point)))
              (filter #(not= (:id %) :background-tile) entities)))

(defn get-entity-at-cursor
  [screen entities input-x input-y]
  (let [point (input->screen screen input-x input-y)]
    (get-entity-at-point entities point)))

(defn set-location-makes
  [entity]
  (assoc entity
        :makes (rand-nth products-list)))

(defn set-location-wants
  [entity entities]
  (let [wants (+ (rand-int 10) 1)
        wants-item (rand-nth (filter #(not= % (:makes entity)) (set (map #(:makes %) entities))))]
    (if wants-item 
      (assoc entity
             :wants wants
             :wants-item wants-item)
      (assoc entity
             :wants 0
             :wants-item ""))))
(defn update-location
  [loc entities]
  (let [loc(if (< (:wants loc) 1)
             (set-location-wants loc entities)
             loc)
        els(for [el (:entities loc)]
            (case (:id el)
              :makes-label (doto el (label! :set-text (format "Makes %s" (name (:makes loc)))))
              :wants-label (doto el (label! :set-text (format "Wants %d %s" (:wants loc) (name (:wants-item loc)))))
              :angry-label (doto el (label! :set-text (format "Angry! %d" (:anger loc))))
              :background (doto el (shape! :set-color (color :red)))
              el))]
    (merge loc (apply bundle els))))

(defn make-location
  [x y]
  (set-location-makes (assoc (bundle (assoc (shape :filled 
                                                   :rect 0 0 loc-width loc-height
                                                   :set-color (color :olive))
                                            :id :background)
                                     (assoc (label "" (color :white))
                                            :x (+ x 5) :y (+ y 15)
                                            :id :makes-label)
                                     (assoc (label "" (color :white))
                                            :x (+ x 5) :id :angry-label)
                                     (assoc (label "" (color :white))
                                            :x (+ x 5) :y (- y 15)
                                            :id :wants-label))
                             :x x 
                             :y y 
                             :box-x x
                             :box-y y
                             :width loc-width
                             :id :temp
                             :height loc-height
                             :location? true
                             :anger 0
                             :wants-item ""
                             :wants 0
                             :center {:x (+ x (/ loc-width 2)) :y (+ y (/ loc-height 2))})))


(defn make-link
  [e1 e2]
  (let [x1 (:x (:center e1))
        y1 (:y (:center e1))
        x2 (:x (:center e2))
        y2 (:y (:center e2))
        lowx (min x1 x2)
        highx (max x1 x2)
        lowy (min y1 y2)
        highy (max y1 y2)
        midx (+ lowx (/ (- highx lowx) 2))
        midy (+ lowy (/ (- highy lowy) 2))]
    (assoc (bundle (shape :line 
                          :line x1 y1 x2 y2
                          :set-color (color :cyan))
                   (shape :filled
                          :circle midx midy circ-radius
                          :set-color (color :cyan)))
           :box-x (- midx (/ circ-radius 2))
           :box-y (- midy (/ circ-radius 2))
           :link? true
           :id :temp
           :height (* circ-radius 2)
           :width (* circ-radius 2)
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

(defn get-locations
  [entities]
  (filter :location? entities))

(defn get-total-links
  [entities]
  (count (filter :link? entities)))

(defn update-entity 
  [screen entities entity]
  (cond 
    (:location? entity)
    (let [connected (connected-to-want entity entities)]
      (if connected
        (update! screen :score (+ (:score screen) 1)))
      (let [entity(if (and connected (:wants entity))
                    (assoc entity 
                           :wants (- (:wants entity) 1)
                           :anger (max (- (:anger entity) 1) 0))
                    (assoc entity :anger (+ (:anger entity) 1)))]
        (update-location entity entities)
      ))
    (= (:id entity) :score)
    (doto entity (label! :set-text (format "Score: %d" (:score screen))))
    :else entity))

(defn play-alert-sound
  [screen entities]
  (sound! (:alert-sound screen) :play)
  entities)

(defn pulse-entities
  [screen entities]
  (let [entities(for [e entities]
                  (update-entity screen entities e))]
    (cond 
      (some #(>= (:anger %) anger-limit) (filter :location? entities))
      (set-screen! ld30 end-screen)
      (some #(>= (:anger %) (- anger-limit 5)) (filter :location? entities))
      (play-alert-sound screen entities)
      :else
      entities
    )))

(defn make-active-link
  [screen entities loc]
  (update! screen :active-link-point (:center loc))
  entities)

(defn add-link
  [screen entities loc active-link-point]
  (update! screen :active-link-point false)
  (sound! (:connect-sound screen) :play)
  (conj entities (make-link loc (get-entity-at-point entities active-link-point)))) 

(defn add-location
  [screen entities pos]
  (update! screen :max-links (+ (:max-links screen) 1))
  (conj entities (update-location (make-location (rectangle! pos :get-x) (rectangle! pos :get-y)) entities)))

(defn generate-location
  [screen entities]
  (let [pos(
            find-first #(= (get-entity-in-box entities %) nil) 
            (take 1000 (repeatedly 
              (fn [] (rectangle (rand-int (- screen-width loc-width)) (rand-int (- screen-height loc-height)) loc-width loc-height)))))]
    (if pos
      (add-location screen entities pos)
      entities)))

(defn remove-link
  [screen entities e]
  (sound! (:remove-sound screen) :play)
  (filter #(= (identical? % e) false) entities))

(defscreen main-screen
  :on-show
  (fn [screen entities]
    (update! screen 
             :score 0
             :max-links 3
             :alert-sound (sound "clip2.wav")
             :remove-sound (sound "remove.wav")
             :connect-sound (sound "connect.wav")
             :renderer (stage))
    (add-timer! screen :pulse 2 2)
    (add-timer! screen :new-loc 15 15)
    (let [locs[(make-location 40 35)
               (make-location 400 150)
               (make-location 250 500)]
          locs(for [l locs]
                (update-location l locs))]
      (cons (assoc (texture "tile1.png")
                   :id :background-tile)
            (conj locs 
                  (assoc (label "Score: 0" (color :white))
                         :id :score
                         :x 15 :y (- screen-height 35))
                  (assoc (label "" (color :white))
                         :id :angriest-label
                         :x 15 :y (- screen-height 50))
                  (assoc (label "" (color :white))
                         :id :links-label
                         :x 15 :y (- screen-height 75))
                  (assoc (shape :filled
                                :rect 0 (- screen-height 100) 210 100
                                :set-color (color :black))
                         :id :score-blocker)
                  ))))

  :on-key-down
  (fn [screen entities]
    (cond
      (= (:key screen) (key-code :a))
      (for [entity entities]
        (if (:location? entity)
        (update-location entity entities) entity))
      (= (:key screen) (key-code :q))
      (app! :exit)
      ))

  :on-touch-down
  (fn [screen entities]
    (let [e (get-entity-at-cursor screen entities (:input-x screen) (:input-y screen))
          active-link-point(:active-link-point screen)
          total-links (get-total-links entities)
          max-links (:max-links screen)]
               (if e
                 (cond
                   (:location? e)
                   (if (and active-link-point (not= active-link-point (:center e)) (< total-links max-links)) 
                     (add-link screen entities e active-link-point)
                     (make-active-link screen entities e))
                   (:link? e)
                   (remove-link screen entities e)
                   :else
                   entities)
                 entities)))

  :on-timer
  (fn [screen entities]
    (case (:id screen)
      :pulse (pulse-entities screen entities)
      :new-loc (generate-location screen entities)
      entities))

  :on-render
  (fn [screen entities]
    (clear!)
    (->> (for [e entities]
           (case (:id e)
             :links-label (doto e (label! :set-text (format "Available links: %d" (- (:max-links screen) (get-total-links entities)))))
             :angriest-label (doto e (label! :set-text (format "Angriest location: %d (max %d)" (apply max (map :anger (get-locations entities))) anger-limit)))
             e))
         (render! screen))))

(defscreen end-screen
  :on-show
  (fn [screen entities]
    (update! screen :renderer (stage))
    [(assoc (label "THE END - press space to retry" (color :white))
            :id :fps
            :x 200 :y 350)])

  :on-key-down
  (fn [screen entities]
    (cond
      (= (:key screen) (key-code :space))
      (set-screen! ld30 main-screen)
      ))

  :on-render
  (fn [screen entities]
    (clear!)
    (render! screen entities)))

(defgame ld30
  :on-create
  (fn [this]
    (set-screen! this main-screen)))
