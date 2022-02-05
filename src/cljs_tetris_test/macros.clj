(ns cljs-tetris-test.macros)

(defmacro with-cur-piece [& body]
  `(do
     (~'draw-cur-piece false)
     ~@body
     (~'draw-cur-piece true)))
