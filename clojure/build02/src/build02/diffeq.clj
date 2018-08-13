(ns build02.diffeq)

(def diff-eq [x xd [{:keys [damping-coefficient gravity mass spring-coefficient]}]]
  (-> x
      (* spring-coefficient)
      (+ (* damping-coefficient xd))
      (/ mass)
      -
      (- gravity)))
