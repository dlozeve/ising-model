(ns ising-model.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))

(defn setup [size]
  (q/frame-rate 300)
  (q/color-mode :hsb)
  (let [matrix (vec (repeatedly (* size size) #(- (* 2 (rand-int 2)) 1)))]
    {:grid-size size
     :matrix matrix
     :beta 10
     :intensity 10
     :iteration 0}))

(defn get-neighbours [state idx]
  [(get (:matrix state) (- idx (:grid-size state)))
   (get (:matrix state) (dec idx))
   (get (:matrix state) (inc idx))
   (get (:matrix state) (+ (:grid-size state) idx))])

(defn hamiltonian [state]
  (- (reduce + (for [i (range (count (:matrix state)))
                     j (filter some? (get-neighbours state i))]
                 (* (:intensity state) ((:matrix state) i) j)))))

(defn toggle-state [state i]
  (let [matrix (:matrix state)]
    (assoc state :matrix (assoc matrix i (* -1 (matrix i))))))

(defn delta-e [state i]
  (* (:intensity state) ((:matrix state) i)
     (reduce + (filter some? (get-neighbours state i)))))

(defn update-state [state]
  (let [i (rand-int (count (:matrix state)))
        new-state (toggle-state state i)
        alpha (q/exp (- (* (:beta state) (delta-e state i))))]
    ;;(println (hamiltonian new-state))
    (update (if (< (rand) alpha) new-state state)
            :iteration inc)))

(defn draw-state [state]
  (q/background 255)
  (let [cell-size (quot (q/width) (:grid-size state))]
    (doseq [[i v] (map-indexed vector (:matrix state))]
      (let [x (* cell-size (rem i (:grid-size state)))
            y (* cell-size  (quot i (:grid-size state)))]
        (q/no-stroke)
        (q/fill
         (if (= 1 v) 0 255))
        (q/rect x y cell-size cell-size))))
  (when (zero? (mod (:iteration state) 50))
    ;;(q/save-frame "img/ising-######.jpg")
    ))

(defn mouse-clicked [state event]
  (setup 100))

(q/defsketch ising-model
  :title "Ising model"
  :size [300 300]
  :setup #(setup 100)
  :update update-state
  :draw draw-state
  :mouse-clicked mouse-clicked
  :features [:keep-on-top :no-bind-output]
  :middleware [m/fun-mode])
 
