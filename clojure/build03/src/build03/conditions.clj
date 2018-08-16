(ns build03.conditions)

(defn termination? [state]
    (>= (:time state) (:tstop state)))

