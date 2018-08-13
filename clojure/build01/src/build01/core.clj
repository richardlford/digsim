(ns build01.core
  (:require [clojure.string :as str])
  (:gen-class))

(def params {
   :damping-coefficient   8.88;  -- Damping force per velocity [N/m/s]
   :gravity               9.88;  -- Acceleration due to gravity [m/sec**2)
   :mass                  1.0;   -- Mass suspended from spring [Kg]
   :spring-coefficient   39.47;  -- Restoring force per position [N/m]
   :x_ic                  0.0;   -- Initial position of suspended mass [m]
   :xd_ic                 0.0;   -- Initial velocity of suspended mass [m/sec]
   :tstop                 2.5
   :dt                   0.01
})

(defn sim []  
  (let [{:keys [damping-coefficient gravity mass spring-coefficient x_ic xd_ic tstop dt]} params]  
    (loop [x x_ic
          xd xd_ic
          time 0.0
          output-lines []]
      (if (> time tstop)
        output-lines
        (let [xdd (-> x 
                      (* spring-coefficient)
                      (+ (* damping-coefficient xd))
                      (/ mass)
                      -
                      (- gravity))]
          (recur (+ x (* xd dt))
                (+ xd (* xdd dt))
                (+ time dt)
                (conj output-lines (str/join " " [(format "%.5e" time) (format "%.5e" x) (format "%.5e" xd)]))))))))

(defn -main
  [& args]
  (doall (map println (sim))))
