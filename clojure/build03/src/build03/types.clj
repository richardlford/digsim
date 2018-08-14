(ns build03.types
    (:require [clojure.string :as str]))

(defrecord State [time x xd])

(defrecord Common [
    damping-coefficient   ;  -- Damping force per velocity [N/m/s]
    gravity               ;  -- Acceleration due to gravity [m/sec**2]
    mass                  ;  -- Mass suspended from spring [Kg]
    spring-coefficient    ;  -- Restoring force per position [N/m]
    tstop
    dt
    dtprint
    state])

(defrecord ParamMapping [name index description])

(def initial-state (->State 0.0 0.0 0.0))
(def default-params (->Common 8.88 9.88 1.0 39.47 2.5 0.01 0.01 initial-state))

(defn state->str 
    [{:keys [time x xd]}]
    (str/join " " [(format "%.5e" time) (format "%.5e" x) (format "%.5e" xd)]))

(def parameter-mappings
   [(->ParamMapping "time"                1  "Simulation time [sec]")
    (->ParamMapping "time0"               2  "Initial time [sec]")
    (->ParamMapping "tstop"               3  "Simulation stop time [sec]")
    (->ParamMapping "dt"                  4  "Integration step size [sec]")
    (->ParamMapping "damping_coefficient" 10 "Damping force per velocity [N/m/s]")
    (->ParamMapping "gravity"             11 "Acceleration due to gravity [m/sec**2]")
    (->ParamMapping "mass"                12 "Mass suspended from spring [Kg]")
    (->ParamMapping "spring_coefficient"  13 "Restoring force per position [N/m]")
    (->ParamMapping "x_ic"                14 "Initial velocity of suspended mass [m/s]")
    (->ParamMapping "xd_ic"               15 "Initial position of suspended mass [m]")
    (->ParamMapping "x"                   16 "Position of suspended mass [m]")
    (->ParamMapping "xd"                  17 "Velocity of suspended mass [m/sec]")
    (->ParamMapping "xdd"                 18 "Acceleration of suspended mass [m/sec**2]")])