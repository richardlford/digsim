(ns digisim.core
  (:require [digisim.build1.core :as build1])
  (:gen-class))

(defn -main
  [& args]
  (let [tstop 2.5
        dt 0.01]
    (doall (map println (build1/sim dt tstop)))))
