(ns digisim.build1
  (:require [clojure.string :as str]))

(def params {
   :damping-coefficient   8.88;  -- Damping force per velocity [N/m/s]
   :gravity               9.88;  -- Acceleration due to gravity [m/sec**2)
   :mass                  1.0;   -- Mass suspended from spring [Kg]
   :spring-coefficient   39.47;  -- Restoring force per position [N/m]
   :x_ic                  0.0;   -- Initial position of suspended mass [m]
   :xd_ic                 0.0;   -- Initial velocity of suspended mass [m/sec]
})

(defn sim
  "Takes a time step and an"  
  [dt tstop]
  (loop [x (:x_ic params)
        xd (:xd_ic params)
        time 0.0
        output-lines []]
    (if (> time tstop)
      output-lines
      (let [xdd (-> x 
                    (* (:spring-coefficient params))
                    (+ (* (:damping-coefficient params) xd))
                    (/ (:mass params))
                    -
                    (- (:gravity params)))]
        (recur (+ x (* xd dt))
               (+ xd (* xdd dt))
               (+ time dt)
               (conj output-lines (str/join " " [time x xd])))))))