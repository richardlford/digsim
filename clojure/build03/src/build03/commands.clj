(ns build03.commands
  (:require [build03.types :refer :all]))

(defmulti process-command (fn [{name :name} loop-params] name))

(defmethod process-command :print [{[name idx] :params} loop-params]
  (if (is-mapped? name (read-string idx))     ; TODO: Add time to print out if it isn't in there already
    (update-in loop-params [:common-params :print-params] conj name)
    (do
      (println "No valid parameter " val " at index " idx)
      loop-params)))

(defmethod process-command :set [{[name idx val] :params} loop-params]
  (if (is-mapped? name (read-string idx))
    (assoc-in loop-params [:common-params (keyword name)] (read-string val))
    (do
      (println "No valid parameter " name " at index " idx)
      loop-params)))

(defmethod process-command :run [_ loop-params]
  (-> loop-params
      (update :current-batch conj (:common-params loop-params))))

(defmethod process-command :stop  [_ loop-params]
  (-> loop-params
      (update :batches conj (:current-batch loop-params))
      (assoc :current-batch [])))

(defmethod process-command :default [{command :name} loop-params]
  (do
    (println "Invalid command:" command)
    loop-params))