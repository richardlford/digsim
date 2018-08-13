(ns build02.core
  (:require [clojure.string :as str]
            [build02.diffeq :refer diff-eq])  
  (:gen-class))

(defrecord State [time
                  x
                  xd])

(defrecord Common [damping-coefficient   ;  -- Damping force per velocity [N/m/s]
                   gravity               ;  -- Acceleration due to gravity [m/sec**2]
                   mass                  ;  -- Mass suspended from spring [Kg]
                   spring-coefficient    ;  -- Restoring force per position [N/m]
                   x_ic
                   xd_ic
                   time0
                   tstop
                   dt
                   state])

(def initial-state ->State [0.0 0.0 0.0])
(def default-params ->Common [8.88 9.88 1.0 39.47 0.0 0.0 2.5 0.01 initial-state])

(defn sim [params]
  (let [{:keys [x_ic xd_ic time0 tstop]}]
    (loop [x x_ic
           xd xd_ic
           time time0
           output-lines []]
      (if (> time tstop)
        output-lines
        (let [xdd (diff-eq x xd def-params)]
          (recur (+ x (* xd dt))
                 (+ xd (* xdd dt))
                 (+ time dt)
                 (conj output-lines (str/join " " [(format "%.5e" time) (format "%.5e" x) (format "%.5e" xd)]))))))))

(defn -main
  [& args]
  (doall (map println (sim))))
