(ns build03.core
  (:require [build03.diffeq :refer [diff-eq]]
            [build03.commands :refer :all]
            [build03.types :refer :all]
            [clojure.string :as str]
            [clojure.java.io :as io])  
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
  (with-open [rdr (io/reader path)]
    (->> rdr
         line-seq
         vec
         (filter (comp not str/blank?))
         (map line->command))))

(defn setup-batches [path]
  (loop [last-command nil
         commands (parse-config path)
         loop-params (->LoopParams default-params [] [])]
    (if (empty? commands)
      (cond
        (= last-command :stop) (:batches loop-params)   ; If the last command was a stop command, then everything is properly formed
        (= last-command :run)  (:batches (process-command (->Command :stop '()) loop-params)) ; Properly close out the last batch
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

(defn sim [params]
  (let [{:keys [state tstop dt]} params]
    (loop [current-state state
           output-lines []]
      (if (> (:time current-state) tstop)
        output-lines
        (let [xdd (diff-eq (:x current-state) (:xd current-state) params)
              {:keys [time x xd]} current-state]
          (recur (apply ->State (mapv + [time x xd] [dt (* xd dt) (* xdd dt)]))
                 (conj output-lines (state->str current-state))))))))

(defn -main
  [& args]
  (-> "input.dat"
      setup-batches
      name-runs))