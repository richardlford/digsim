(ns digisim.build2
  (:require [clojure.string :as str]))

(defrecord SimParams [mass gravity damping-coefficient spring-coefficient x_ic xd_ic])

(def default-params (->SimParams 1.0 9.88 8.88 39.47 0.0 0.0))

(defrecord SimState [x xd time])

(defn state->str [{:keys [time x xd]} state]
  (str/join " " [time x xd]))

(defn sim
  "Takes a time step and an"  
  [dt tstop]
  (loop [x (:x_ic params)
        xd (:xd_ic params)
        time 0.0
        output-states []]
    (if (> time tstop)
      output-states
      (let [xdd (-> x 
                    (* (:spring-coefficient params))
                    (+ (* (:damping-coefficient params) xd))
                    (/ (:mass params))
                    -
                    (- (:gravity params)))]
        (recur (+ x (* xd dt))
               (+ xd (* xdd dt))
               (+ time dt)
               (conj output-states ))))))