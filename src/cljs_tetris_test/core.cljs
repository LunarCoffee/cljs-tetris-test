(ns cljs-tetris-test.core
  (:require
   [reagent.core :as r]
   [reagent.dom :as d])
  (:require-macros
   [cljs-tetris-test.macros :as mc]))

(def field (r/atom (vec (repeat 20 (vec (repeat 10 "black"))))))
(def cur-piece (atom nil))

(def lock-delay-timer (atom 0))

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

(defn piece-has-space [old new coord-bounds]
  (and (every? coord-bounds (last new))
       (every? #(or (some #{(get-in @field %)} ["black" "#222"])
                    (some #{%} (last old)))
               (last new))))

(defn piece-can-fall [piece]
  (let [updated (move-piece piece 0 inc)]
    (piece-has-space piece updated #(< (first %) 20))))

(defn get-shadow-piece [piece]
  (if (piece-can-fall piece)
    (recur (move-piece piece 0 inc))
    (assoc piece 0 :shadow)))

(defn draw-piece [piece on]
  (let [color (if on (piece-color (first piece)) "black")]
    (doseq [coords (last piece)]
      (swap! field assoc-in coords color))))

(defn draw-cur-piece [on]  ;; used in macros.clj (FIXME ?)
  (draw-piece (get-shadow-piece @cur-piece) on)
  (draw-piece @cur-piece on))

(defn hard-drop-cur-piece []
  (when (some? @cur-piece)
    (while (piece-can-fall @cur-piece)
      (mc/with-cur-piece (swap! cur-piece move-piece 0 inc))))
  (spawn-piece))

(defn frame-update []
  (when (> @lock-delay-timer 0)
    (swap! lock-delay-timer dec)
    (when (zero? @lock-delay-timer)
      (hard-drop-cur-piece))))

(defn frame-drop-cur-piece []
  (when (some? @cur-piece)
    (if (piece-can-fall @cur-piece)
      (mc/with-cur-piece (swap! cur-piece move-piece 0 inc))
      (when (zero? @lock-delay-timer)
        (reset! lock-delay-timer 30)))))

(defn move-cur-piece-x [dir]
  (when (some? @cur-piece)
    (let [updated (move-piece @cur-piece 1 dir)]
      (when (piece-has-space @cur-piece updated #(< -1 (last %) 10))
        (mc/with-cur-piece (reset! cur-piece updated))))))

(defn piece-center-coords [piece]
  (if (= :s (first piece)) 3 2))  ;; non-standard pivot for i piece :(

(defn coord- [[a b] [c d]] [(- a c) (- b d)])
(defn coord+ [[a b] [c d]] [(+ a c) (+ b d)])

(defn coord-rotate-cw [[a b]] [b (- a)])
(defn coord-rotate-ccw [[a b]] [(- b) a])

(defn rotate-piece [piece dir]
  (let [center (get (vec (last piece)) (piece-center-coords piece))]
    (update piece 1 (partial map #(coord+ center (dir (coord- % center)))))))

(defn rotate-cur-piece [dir]
  (when (and (some? @cur-piece) (not= :o (first @cur-piece)))
    (let [updated (rotate-piece @cur-piece dir)]
      (when (piece-has-space @cur-piece updated
                             #(and (< -1 (last %) 10)
                                   (< (first %) 20)))
        (mc/with-cur-piece (reset! cur-piece updated))))))

(defn flip-cur-piece []
  (rotate-cur-piece coord-rotate-cw)
  (rotate-cur-piece coord-rotate-cw))

;; timers

(defonce frame-updater (js/setInterval frame-update 16))
(defonce cur-piece-updater (js/setInterval frame-drop-cur-piece 250))

;; event handlers

(defn event-key-down [e]
  (case (.-key e)
    "ArrowLeft" (move-cur-piece-x dec)
    "ArrowRight" (move-cur-piece-x inc)
    "ArrowUp" (rotate-cur-piece coord-rotate-cw)
    "s" (rotate-cur-piece coord-rotate-ccw)
    "a" (flip-cur-piece)
    " " (hard-drop-cur-piece)
    "f" (.log js/console (str (move-piece nil 1 inc)))
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
