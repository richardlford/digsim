(ns build03.core
  (:require [build03.diffeq :refer [diff-eq]]
            [build03.types :refer :all]
            [clojure.string :as str]
            [clojure.java.io :as io])  
  (:gen-class))

(defrecord LoopParams [common-params current-batch batches]

(defmulti process-command (fn [[command & args]] (-> command str/lower-case keyword)))

(defmethod process-command :print [[_ name idx] loop-params]
  (if (is-mapped? name (read-string idx))     ; TODO: Add time to print out if it isn't in there already
    (update-in loop-params [:common-params :print-params] conj name)
    (do
      (println "No valid parameter " val " at index " idx)
      loop-params)))

(defmethod process-command :set   [[_ name idx val] loop-params]
  (if (is-mapped? val (read-string idx)
    (update-in loop-params [:common-params (keyword name)] #((read-string val))))
    (do
      (println "No valid parameter " val " at index " idx)
      loop-params)))))

(defmethod process-command :run [_ loop-params] 
  (-> loop-params
    (update :current-batch conj (:common-params loop-params))
    (assoc :current-batch [])))

(defmethod process-command :stop  [_ loop-params] 
  (-> loop-params
    (update :batches conj (:current-batch loop-params))
    (assoc :current-batch [])))

(defmethod process-command :default [[command & _] loop-params] 
  (do 
    (println "Invalid command:" command)
    loop-params))

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
