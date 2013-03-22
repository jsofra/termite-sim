(ns termite-sim.quil-utils
  (:use quil.core))

(defmacro with-style [style & body]
  `(do (push-style)
       (try
         (doseq [[style-fn# args#] ~style]
           (apply style-fn# args#))
         ~@body
         (finally (pop-style)))))
