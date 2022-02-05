(ns cljs-tetris-test.core
  (:require
   [reagent.core :as r]
   [reagent.dom :as d])
  (:require-macros
   [cljs-tetris-test.macros :as mc]))

(def field (r/atom (vec (repeat 20 (vec (repeat 10 "black"))))))
(def cur-piece (atom nil))

(defn rand-piece-kind []
  (rand-nth [:i :o :s :z :j :l :t]))

(defn piece-start-pos [piece-kind]
  (case piece-kind
    :i [[0 3] [0 4] [0 5] [0 6]]
    :o [[0 4] [0 5] [1 4] [1 5]]
    :s [[0 4] [0 5] [1 3] [1 4]]
    :z [[0 3] [0 4] [1 4] [1 5]]
    :j [[0 3] [1 3] [1 4] [1 5]]
    :l [[0 5] [1 3] [1 4] [1 5]]
    :t [[0 4] [1 3] [1 4] [1 5]]))

(defn piece-color [piece-kind]
  (case piece-kind
    :i "cyan"
    :o "yellow"
    :s "lime"
    :z "red"
    :j "blue"
    :l "orange"
    :t "magenta"
    :shadow "#222"))

(defn spawn-piece []
  (let [kind (rand-piece-kind)]
    (reset! cur-piece [kind (piece-start-pos kind)])))

(defn move-piece [piece dir shift-fn]
  (update piece 1 (partial map #(update % dir shift-fn))))

(defn space-available-for-piece [old new coord-bounds]
  (and (every? coord-bounds (map last (last new)))
       (every? #(or (some #{(get-in @field %)} ["black" "#222"])
                    (some #{%} (last old)))
               (last new))))

(defn piece-can-fall [piece]
  (let [updated (move-piece piece 0 inc)]
    (space-available-for-piece piece updated #(< % 20))))

(defn get-shadow-piece [piece]
  (if (piece-can-fall piece)
    (recur (move-piece piece 0 inc))
    (assoc piece 0 :shadow)))

(defn draw-piece [piece on]
  (let [color (if on (piece-color (first piece)) "black")]
    (doseq [coords (last piece)]
      (swap! field assoc-in coords color))))

;; draw the current piece and its shadow
(defn update-field-cur-piece [on]  ;; used in macros.clj (FIXME ?)
  (if (some? @cur-piece)
    (do
      (draw-piece (get-shadow-piece @cur-piece) on)
      (draw-piece @cur-piece on))
    (spawn-piece)))

(defn frame-update-cur-piece []
  (if (some? @cur-piece)
    (if (piece-can-fall @cur-piece)
      (mc/with-cur-piece (swap! cur-piece move-piece 0 inc))
      (reset! cur-piece nil))
    (spawn-piece)))

(defn move-cur-piece-x [dir]
  (let [updated (move-piece @cur-piece 1 dir)]
    (when (space-available-for-piece @cur-piece updated #(< -1 % 10))
      (mc/with-cur-piece (reset! cur-piece updated)))))

(defn cur-piece-center-coords []
  (if (= :s (first @cur-piece)) 3 2))  ;; non-standard pivot for i piece :(

(defn coord- [[a b] [c d]] [(- a c) (- b d)])
(defn coord+ [[a b] [c d]] [(+ a c) (+ b d)])

(defn coord-rotate-cw [[a b]] [b (- a)])
(defn coord-rotate-ccw [[a b]] [(- b) a])

(defn rotate-cur-piece [dir]
  (when (not= :o (first @cur-piece))
    (let [center (get (vec (last @cur-piece)) (cur-piece-center-coords))]
      (mc/with-cur-piece
        (swap! cur-piece update 1
               (partial map #(coord+ center (dir (coord- % center)))))))))

(defn flip-cur-piece []
  (rotate-cur-piece coord-rotate-cw)
  (rotate-cur-piece coord-rotate-cw))

(defn hard-drop-cur-piece []
  (while (piece-can-fall @cur-piece)
    (mc/with-cur-piece (swap! cur-piece move-piece 0 inc)))
  (spawn-piece))

;; timers

(defonce cur-piece-updater (js/setInterval frame-update-cur-piece 200))

;; event handlers

(defn event-key-down [e]
  (case (.-key e)
    "ArrowLeft" (move-cur-piece-x dec)
    "ArrowRight" (move-cur-piece-x inc)
    "ArrowUp" (rotate-cur-piece coord-rotate-cw)
    "s" (rotate-cur-piece coord-rotate-ccw)
    "a" (flip-cur-piece)
    " " (hard-drop-cur-piece)
    (.log js/console (str "unhandled key " (.-key e)))))

;; components

(defn block [color]
  [:div {:style {:background-color color}
         :class "block"}])

(defn playfield-row [row]
  [:div {:class "playfield-row"}
   (for [[i cell] (map-indexed vector row)]
     ^{:key i} [block cell])])

(defn playfield [field]
  [:div {:class "playfield"
         :tab-index 0
         :on-key-down event-key-down}
   (for [[i row] (map-indexed vector field)]
     ^{:key i} [playfield-row row])])

;; views

(defn home-page []
  [:div
   [:h2 "tetris gaming"]
   [playfield @field]])

;; initialization

(defn mount-root []
  (spawn-piece)
  (d/render [home-page] (.getElementById js/document "app")))

(defn ^:export init! []
  (mount-root))
