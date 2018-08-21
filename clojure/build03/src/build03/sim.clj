(ns build03.sim
    (:require '[build03.diffeq :as diffeq]
              '[build03.types :refer :all]
              '[clojure.java.io :as io]))

(defn print-state [state [& ks]]
    (->> state
        ((apply juxt ks))
        (map (partial format "%.5e"))
        (str/join " ")))
           

(defn termination? [params state]
    (>= (:time state) (:tstop params)))

(defn run [params output-name]
    (with-open [outfile (io/writer output-name)]
        ))