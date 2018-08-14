(ns build02.diffeq)

(defn diff-eq [x xd {:keys [damping-coefficient gravity mass spring-coefficient]}]
  (-> x
      (* spring-coefficient)
      (+ (* damping-coefficient xd))
      (/ mass)
      -
      (- gravity)))
