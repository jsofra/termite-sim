(ns termite-sim.sim
  (:use quil.core)
  (:use termite-sim.quil-utils))

(defn world [w h]
  "Create a grid (nested vectors) of refs containing empty maps."
     (into [] (for [x (range w)]
                (into [] (for [y (range h)]
                           (ref {}))))))

(defn world-dims [world]
  "Return a vector [w h] containing the dimensions of the world."
  [(count (first world)) (count world)])

(defn rand-world-loc [world]
  "Return a random locations within the world."
  (mapv rand-int (world-dims world)))

(defn create-termite [world loc]
  "Create a termite at the location, returning a termite agent on the location."
  (dosync
   (alter (get-in world loc) assoc :termite {})
   (agent loc)))

(defn setup-world [world & {:keys [woodchips termites]}]
  "Add some woodchips and termites to the world."
  (dosync
   (dotimes [_ woodchips]
     (let [loc (rand-world-loc world)]
       (alter (get-in world loc) assoc :woodchip true)))
   (doall
    (for [_ (range termites)]
      (let [loc (rand-world-loc world)]
        (create-termite world loc))))))

(defn loc-in-dir [dims loc dir]
  "Find a neighbouring locaion in one of four directions.
   Confines the coordinate to a torus."
  (let [dirs [[1 0] [0 1] [-1 0] [0 -1]]]
    (mapv mod (mapv + (dirs dir) loc) dims)))

;; termite behaviour ;;

(defn walk [world loc new-loc]
  "Moves a termite from one cell to another.
   Must run in a transaction."
  (let [dims (world-dims world)
        old-cell (get-in world loc)
        new-cell (get-in world new-loc)
        termite (:termite @old-cell)]
    (alter new-cell assoc :termite termite)
    (alter old-cell dissoc :termite)
    new-loc))

(defn drop-chip [world loc]
  "Moves a chip from the termite into the cell.
   Must run in a transaction."
  (let [cell (get-in world loc)
        termite (:termite @cell)]
    (alter cell assoc :woodchip true)
    (alter cell assoc :termite (dissoc termite :woodchip))
    loc))

(defn pickup-chip [world loc nloc]
  "Moves a chip from a neighbouring cell into a termite.
   Must run in a transaction."
  (let [cell (get-in world loc)
        ncell (get-in world nloc)
        termite (:termite @cell)]
    (alter ncell dissoc :woodchip)
    (alter cell assoc :termite (assoc termite :woodchip true))
    loc))

(defn empty-cell? [cell]
  (let [{termite :termite woodchip :woodchip} cell]
    (not (or termite woodchip))))

(defn forage [world termite]
  (letfn [(inner-forage [loc]
            (let [cell (get-in world loc)
                  woodchip (:woodchip @cell)
                  has-chip? (get termite :woodchip)
                  nloc (loc-in-dir (world-dims world) loc (rand-int (count dirs)))
                  ncell (get-in world nloc)
                  {ntermite :termite nwoodchip :woodchip} @ncell]
              (Thread/sleep 40)
              (dosync
               (cond
                (or (empty-cell? @ncell) (and (not ntermite) woodchip)) (walk world loc nloc)
                (and nwoodchip (not has-chip?) (not ntermite)) (pickup-chip world loc nloc)
                (and nwoodchip (not ntermite) has-chip?) (drop-chip world loc)
                :else loc)
               )))]
    (send-off termite inner-forage)))

;;; GUI ;;;

(defn setup []
  (let [sim-world (world 80 80)
        termites (setup-world sim-world
                              :woodchips 250
                              :termites 50)]
    (set-state! :world sim-world
                :termites termites)

    ;; add watches to loop the termites
    (dorun (map #(add-watch % :forage (fn [k term os ns]
                                        (forage sim-world term))) termites))
    ;; kill off the loops
    (dorun (map #(forage sim-world %) termites))

    (smooth)
    (frame-rate 25)))

(defn render-cell [cell loc]
  (with-translation loc
    (let [col (cond
               (get-in cell [:termite :woodchip]) [255 0 0]
               (:woodchip cell) [139 69 19]
               (:termite cell) [173 255 47]
               :else false)]
      (when col
        (with-style
         {fill col}
         (ellipse 4 4 10 10))))))

(defn draw []
  (let [sim-world (state :world)
        [w h] (world-dims sim-world)
        termites (state :termites)
        cells (dosync (into [] (for [x (range w) y (range h)]
                                 [@(get-in sim-world [x y]) [(* 10 x) (* 10 y)]])))]
    (with-style
      {background [200]
       stroke [0 0 0]}

      (doseq [cell cells]
        (apply render-cell cell)))))
