(ns build03.events
  (:require '[build03.types :refer :all]
            '[clojure.string :as str]))

(defn print-state [state [& ks]]
  (->> state
       ((apply juxt ks))
       (map (partial format "%.5e"))
       (str/join " ")))

(defmulti handle-event (fn [{name :name} sim-params output-lines] name))

(defmethod handle-event :log-state [_ {state :state print-params :print-params} output-lines]
  (conj output-lines (print-state state print-params)))

; I don't think we need this event... yet
(defmethod handle-event :terminate [_ sim-params _]
  (do
    (println "############################################################")
    (println "################ terminate event called ####################")
    (println "############################################################")
    sim-params))

;; Don't call discrete yet because we don't have it set up
(defmethod handle-event :bounce [_ sim-params _]
  (do
    (println "############################################################")
    (println "################## bounce event called #####################")
    (println "############################################################")
    sim-params))

(defmethod handle-event :noop [_ sim-params _]
  (do
    (println "############################################################")
    (println "################## noop event called #######################")
    (println "############################################################")    
    sim-params))

(defmethod handle-event :default [{name :name, time :time} sim-params]
  (do 
    (println "Unknown event: " name " at " time " secs")
    sim-params))