(ns cljs-tetris-test.macros)

(defmacro with-cur-piece [& body]
  `(do
     (~'update-field-cur-piece false)
     ~@body
     (~'update-field-cur-piece true)))
