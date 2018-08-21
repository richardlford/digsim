(ns build03.core
  (:require [build03.diffeq :refer [diff-eq]]
            [build03.types :refer :all]
            [clojure.string :as str]
            [clojure.java.io :as io])  
  (:gen-class))

(defrecord LoopParams [common-params current-batch batches]

(defmulti process-command (fn [[command & args]] (-> command str/lower-case keyword)))

(defmethod process-command :print [[_ val idx] loop-params]
  (if (some #(and (= (:name val)) (= (:index idx))) parameter-mappings)
    (update-in conj )
  ))
(defmethod process-command :set   [_ & args ] 1)
(defmethod process-command :run   [_] 2)
(defmethod process-command :stop  [_] 3)
(defmethod process-command :default [[command & _] & _] (println "Invalid command:" command))

(defn reset [params]
  "Clears out values to plot and print, and sets recalc to false. In Ada version, also clears states, and sets quit and over to false."
  (-> params
    (assoc :plots [])
    (assoc :print-params [])
    (assoc :recalc false)))



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
  ())
