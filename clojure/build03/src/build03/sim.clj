(ns build03.sim
    (:require '[build03.diffeq :as diffeq]
              '[build03.types :refer :all]
              '[clojure.java.io :as io]))

(defn is-done? [params state]
    (>= (:time state) (:tstop params)))

(defn run [params]
    (let [{:keys [state tstop dt]} params]
      (loop [current-state state
             output-lines []]
        (if (is-done? params state))
          output-lines
          (let [xdd (diff-eq (:x current-state) (:xd current-state) params)
                {:keys [time x xd]} current-state]
            (recur (apply ->State (mapv + [time x xd] [dt (* xd dt) (* xdd dt)]))
                   (conj output-lines (state->str current-state)))))))