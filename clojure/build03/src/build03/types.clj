(ns build03.types
    (:require [clojure.string :as str]))

(defrecord State [time x xd])

(defrecord Common [damping-coefficient   ;  -- Damping force per velocity [N/m/s]
     gravity               ;  -- Acceleration due to gravity [m/sec**2]
     mass                  ;  -- Mass suspended from spring [Kg]
     spring-coefficient    ;  -- Restoring force per position [N/m]
     tstop
     dt
     state])

(def initial-state (->State 0.0 0.0 0.0))
(def default-params (->Common 8.88 9.88 1.0 39.47 2.5 0.01 initial-state))

(defn state->str 
    [{:keys [time x xd]}]
    (str/join " " [(format "%.5e" time) (format "%.5e" x) (format "%.5e" xd)]))