(ns termite-sim.main
  (:use quil.core)
  (:require [termite-sim.sim :as sim])
  (:gen-class))

(defn -main [& args]
  (println "Start termite sim!")
  (defsketch termite-sketch
    :title "Termite Sim!"
    :setup sim/setup
    :draw sim/draw
    :size [800 800]))
