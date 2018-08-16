(ns build03.core
  (:require [build03.diffeq :refer [diff-eq]]
            [build03.types :refer :all]
            [clojure.string :as str]
            [clojure.java.io :as io])  
  (:gen-class))


(defn run-driver [config output-name]
  (loop [state (:initial-state config)]
        (if-not ((:terminate-fn config) state)
         (recur (swap! state advance-state)))))

(defn lines->run [coll] )

(defn lines->batch [coll] 
  (loop [lines coll
         [run rest] (split-with)]
    ))

(defn config->lines [path]
  (with-open [rdr (io/reader path)]
    (line-seq rdr)))

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
