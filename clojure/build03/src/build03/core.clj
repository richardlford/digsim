(ns build03.core
  (:require [build03.commands :refer :all]
            [build03.types :refer :all]
            [build03.sim :as sim]
            [clojure.string :as str])  
  (:gen-class))

(defn reset [params]
  "Clears out values to plot and print, and sets recalc to false. In Ada version, also clears states, and sets quit and over to false."
  (-> params
    (assoc :plots [])
    (assoc :print-params [])
    (assoc :recalc false)))

(defn line->command [line]
  (let [tokens (str/split line #"\s+")
        to-parse (take-while #(not (str/starts-with? % "#")) tokens)
        name (keyword (first to-parse))
        params (rest to-parse)]
    (->Command name params)))

(defn parse-config [path]
  (with-open [rdr (clojure.java.io/reader path)]
    (->> rdr
         line-seq
         vec
         (filter (comp not str/blank?))
         (map line->command))))

(defn setup-batches [path]
  (loop [prev-command nil
         commands (parse-config path)
         loop-params (->LoopParams default-params [] [])]
    (if (empty? commands)
      (cond
        (= prev-command :stop) (:batches loop-params)   ; If the last command was a stop command, then everything is properly formed
        (= prev-command :run)  (:batches (process-command (->Command :stop '()) loop-params)) ; Properly close out the last batch
        :else (:batches (->> loop-params (process-command (->Command :run '())) (process-command (->Command :stop '()))))) ; Same as above
      (recur (:name (first commands)) (rest commands) (process-command (first commands) loop-params)))))

(defn name-runs [batches]
  (if (and (= 1 (count batches)) (= 1 (count (first batches))))
    [(assoc (ffirst batches) :output-name "output.dat")]
    (apply concat
      (map-indexed 
        (fn [i batch] 
          (map-indexed 
            (fn [j run] (assoc run :output-name (str "output" (inc i) "." (inc j) ".dat")))
           batch))
        batches))))


(defn -main
  [& args]
  (->> "input.dat"
       setup-batches
       name-runs
       (doall sim/run)))