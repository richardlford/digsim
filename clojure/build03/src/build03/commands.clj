(ns build03.commands
    (:require '[clojure.string :as str]))

(defmulti process-command (fn [[command & args]] (-> command str/lower-case keyword)))

(defmethod process-command :print [[_ & args]] args)
(defmethod process-command :set   [_] 1)
(defmethod process-command :run   [_] 2)
(defmethod process-command :stop  [_] 3)
(defmethod process-command :default [args] (println "Invalid command:" args))