(ns cljs-tetris-test.core
  (:require
   [reagent.core :as r]
   [reagent.dom :as d])
  (:require-macros
   [cljs-tetris-test.macros :as mc]))

(defn rvec [n item]
  (vec (repeat n item)))

(def field (r/atom (rvec 20 (rvec 10 "black"))))

(def cur-piece (atom nil))
(def held-piece (r/atom nil))
(def rotation-state (atom 0))

(def drop-delay-timer (atom 0))
(def lock-delay-timer (atom 0))

(def drop-delay (atom 15))

(def auto-shift-delay 110)
(def auto-repeat-rate 0)
(def soft-drop-factor ##Inf)

(def move-key-pressed (atom false))
(def move-key-dir (atom nil))

(defn gen-7-bag []
  (shuffle [:i :o :s :z :j :l :t]))

(def cur-bag (r/atom []))
(def next-bag (r/atom (gen-7-bag)))

(defn rand-piece-kind []
  (when (empty? @cur-bag)
    (reset! cur-bag @next-bag)
    (reset! next-bag (gen-7-bag)))
  (last (first (swap-vals! cur-bag pop))))

(defn peek-bag [n]
  (reverse (take-last n (concat @next-bag @cur-bag))))

(defn piece-start-pos [piece-kind]
  (case piece-kind
    :i [[0 3] [0 4] [0 5] [0 6]]
    :o [[0 4] [0 5] [1 4] [1 5]]
    :s [[0 4] [0 5] [1 3] [1 4]]
    :z [[0 3] [0 4] [1 4] [1 5]]
    :j [[0 3] [1 3] [1 4] [1 5]]
    :l [[0 5] [1 3] [1 4] [1 5]]
    :t [[0 4] [1 3] [1 4] [1 5]]))

(defn piece-bounding-box-size [piece-kind]
  (let [start-pos (piece-start-pos piece-kind)
        row (mapv first start-pos)
        col (mapv last start-pos)]
    [(- (apply max row) (apply min row) -1)
     (- (apply max col) (apply min col) -1)]))

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
    (reset! cur-piece [kind (piece-start-pos kind)])
    (reset! rotation-state 0)))

(defn reset-game []
  (reset! field (rvec 20 (rvec 10 "black")))
  (reset! held-piece nil)
  (reset! drop-delay-timer 0)
  (reset! lock-delay-timer 0)
  (reset! cur-bag [])
  (reset! next-bag (gen-7-bag))
  (spawn-piece))

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

(defn project-piece-down [piece]
  (if (piece-can-fall piece)
    (recur (move-piece piece 0 inc))
    piece))

(defn get-shadow-piece [piece]
  (assoc (project-piece-down piece) 0 :shadow))

(defn draw-piece [piece on]
  (let [color (if on (piece-color (first piece)) "black")]
    (doseq [coords (last piece)]
      (swap! field assoc-in coords color))))

(defn draw-cur-piece [on]  ;; used in macros.clj (FIXME ?)
  (draw-piece (get-shadow-piece @cur-piece) on)
  (draw-piece @cur-piece on))

(defn clear-lines []
  (let [remaining (remove (partial every? #(not= "black" %)) @field)
        n-cleared (- 20 (count remaining))]
    (reset! field (vec (concat (rvec n-cleared (rvec 10 "black")) remaining)))))

(defn hard-drop-cur-piece []
  (when (some? @cur-piece)
    (while (piece-can-fall @cur-piece)
      (mc/with-cur-piece (swap! cur-piece move-piece 0 inc))))
  (clear-lines)
  (spawn-piece))

(defn frame-drop-cur-piece []
  (when (some? @cur-piece)
    (if (piece-can-fall @cur-piece)
      (mc/with-cur-piece (swap! cur-piece move-piece 0 inc))
      (when (zero? @lock-delay-timer)
        (reset! lock-delay-timer 30)))))

(defn frame-update []
  (when (> @lock-delay-timer 0)
    (swap! lock-delay-timer dec)
    (when (zero? @lock-delay-timer)
      (hard-drop-cur-piece)))
  (case @drop-delay-timer
    0 (do
        (frame-drop-cur-piece)
        (reset! drop-delay-timer @drop-delay))
    (swap! drop-delay-timer dec)))

(defn move-cur-piece-x [dir repeat]
  (when (some? @cur-piece)
    (let [updated (move-piece @cur-piece 1 dir)]
      (when (and
             @move-key-pressed
             (piece-has-space @cur-piece updated #(< -1 (last %) 10)))
        (mc/with-cur-piece (reset! cur-piece updated))
        (when repeat
          (if (zero? auto-repeat-rate)
            (recur dir true)
            (js/setTimeout #(move-cur-piece-x dir true) auto-repeat-rate)))))))

(defn piece-center-coords [piece]
  (case (first piece)
    :s (get (vec (last piece)) 3)
    :i (let [second-block (get (vec (last piece)) 1)]
         (map + second-block
              (case @rotation-state
                0 [0.5 0.5]
                1 [0.5 -0.5]
                2 [-0.5 -0.5]
                3 [-0.5 0.5])))
    (get (vec (last piece)) 2)))

(defn coord- [[a b] [c d]] [(- a c) (- b d)])
(defn coord+ [[a b] [c d]] [(+ a c) (+ b d)])

(defn coord-rotate-cw [[a b]] [b (- a)])
(defn coord-rotate-ccw [[a b]] [(- b) a])

(defn rotate-piece [piece dir]
  (let [center (piece-center-coords piece)]
    (update piece 1 (partial map #(coord+ center (dir (coord- % center)))))))

;; srs
(defn piece-kick-table [piece-kind state dir]
  (let [from-state (if (zero? dir) state (mod (- state 1) 4))
        kicks (if (= piece-kind :i)
                (case from-state
                  0 [[0 0] [0 -2] [0 1] [-1 -2] [2 1]]
                  1 [[0 0] [0 -1] [0 2] [2 -1] [-1 2]]
                  2 [[0 0] [0 2] [0 -1] [1 2] [-2 -1]]
                  [[0 0] [0 1] [0 -2] [-2 1] [1 -2]])
                (case from-state
                  0 [[0 0] [0 -1] [1 -1] [-2 0] [-2 -1]]
                  1 [[0 0] [0 1] [-1 1] [2 0] [2 1]]
                  2 [[0 0] [0 1] [1 1] [-2 0] [-2 1]]
                  [[0 0] [0 -1] [-1 -1] [2 0] [2 -1]]))]
    (if (zero? dir) kicks (map #(map - %) kicks))))

(defn rotate-cur-piece [dir]
  (when (and (some? @cur-piece) (not= :o (first @cur-piece)))
    (let [rotated (rotate-piece @cur-piece dir)
          direction (if (= dir coord-rotate-cw) 0 1)
          bounds-pred #(and (< -1 (last %) 10) (< (first %) 20))
          kicks (piece-kick-table (first rotated) @rotation-state direction)
          kicked (->> kicks
                      (map (fn [[row-kick col-kick]]
                             (-> rotated
                                 (move-piece 0 #(- % row-kick))
                                 (move-piece 1 #(+ % col-kick)))))
                      (filter #(piece-has-space @cur-piece % bounds-pred))
                      (first))]
      (when (some? kicked)
        (mc/with-cur-piece (reset! cur-piece kicked))
        (swap! rotation-state #(mod ((if (= direction 0) inc dec) %) 4))
        (when @move-key-pressed
          (move-cur-piece-x @move-key-dir true))))))

(defn flip-cur-piece []
  (rotate-cur-piece coord-rotate-cw)
  (rotate-cur-piece coord-rotate-cw))

(defn swap-hold-piece []
  (mc/with-cur-piece
    (let [cur-held @held-piece
          cur-kind (first @cur-piece)]
      (if (some? cur-held)
        (reset! cur-piece @held-piece)
        (spawn-piece))
      (reset! held-piece [cur-kind (piece-start-pos cur-kind)]))))

(def drop-delay-regular 15)
(def soft-drop-activated (atom false))

(defn activate-soft-drop []
  (if (= ##Inf soft-drop-factor)
    (mc/with-cur-piece (reset! cur-piece (project-piece-down @cur-piece)))
    (do
      (when (= @drop-delay drop-delay-regular)
        (reset! drop-delay-timer 0))
      (when (not @soft-drop-activated)
        (swap! drop-delay #(/ % soft-drop-factor))
        (reset! soft-drop-activated true)))))

(defn deactivate-soft-drop []
  (reset! soft-drop-activated false)
  (reset! drop-delay drop-delay-regular))

(defonce frame-updater (js/setInterval frame-update 16))

;; event handlers

(defn das-move-x [dir]
  (reset! move-key-pressed true)
  (reset! move-key-dir dir)
  (move-cur-piece-x dir false)
  (js/setTimeout #(move-cur-piece-x dir true) auto-shift-delay))

(defn stop-move []
  (reset! move-key-pressed false))

(defn event-key-down [e]
  (case (.-key e)
    "ArrowLeft" (das-move-x dec)
    "ArrowRight" (das-move-x inc)
    "ArrowUp" (rotate-cur-piece coord-rotate-cw)
    "ArrowDown" (activate-soft-drop)
    "s" (rotate-cur-piece coord-rotate-ccw)
    "a" (flip-cur-piece)
    "d" (swap-hold-piece)
    " " (hard-drop-cur-piece)
    "`" (reset-game)
    (.log js/console (str "unhandled key " (.-key e)))))

(defn event-key-up [e]
  (case (.-key e)
    "ArrowLeft" (stop-move)
    "ArrowRight" (stop-move)
    "ArrowDown" (deactivate-soft-drop)
    ()))

;; components

(defn block [color]
  [:div {:style {:background-color color}
         :class (if (= color "black") "block-empty" "block")}])

(defn piece-field-row [row]
  [:div {:class "playfield-row"}
   (for [[i cell] (map-indexed vector row)]
     ^{:key i} [block cell])])

(defn piece-field [field class]
  [:div {:class class
         :tab-index 0
         :on-key-down event-key-down
         :on-key-up event-key-up}
   (for [[i row] (map-indexed vector field)]
     ^{:key i} [piece-field-row row])])

(defn single-piece-field [piece class]
  (if (some? piece)
    (let [piece-kind (first piece)
          shift (if (= piece-kind :o) 4 3)  ;; to move coords to start at 0
          [row-bound col-bound] (piece-bounding-box-size piece-kind)
          single-field (reduce
                        #(assoc-in %1 %2 (piece-color piece-kind))
                        (rvec row-bound (rvec col-bound "black"))
                        (map (fn [[row col]] [row (- col shift)]) (last piece)))]
      [piece-field single-field class])
    [:div {:class class}]))

(defn next-queue [peek-n]
  (let [queue (peek-bag peek-n)]
    [:div {:class "next-queue"}
     (for [[i kind] (map-indexed vector queue)]
       ^{:key i} [single-piece-field [kind (piece-start-pos kind)] "queue-piece"])]))

;; views

(defn home-page []
  [:div
   [:h2 "tetris gaming!!1!"]
   [:div {:class "game-container"}
    [:div
     [:p "hold:"]
     [single-piece-field @held-piece "held-field"]]
    [piece-field @field "playfield"]
    [:div
     [:p "next:"]
     [next-queue 5]]]])

;; initialization

(defn mount-root []
  (spawn-piece)
  (d/render [home-page] (.getElementById js/document "app")))

(defn ^:export init! []
  (mount-root))
