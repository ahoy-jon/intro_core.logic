(ns cld.custom
    (:require [clojure.core.logic :refer :all]
              [clojure.core.logic.protocols :refer [walk]]))





(def mydata [1 2, 1 5, 2 5, 3 2, 5 4, 5 1, 6 4])


(defn mygoalo [lvar1 lvar2]
  (fn [sub]
    (let [lvar1 (walk sub lvar1)
          lvar2 (walk sub lvar2)
          ]


      (cond
       (lvar? lvar1) (unify sub lvar1 "l1")
       (lvar? lvar2) (unify sub lvar2 "l2")
       :else (when
               (= lvar1 lvar2) sub)))))



(run* [a b c]
      (== a 1)
      (mygoalo b c))


(run* [a b]
      (== b 1)
      (== a b)
      (mygoalo a b))


(defn sometingo [n1 n2]
   (all
    (== n1 n2)
    (!= n1 1)))



(defn my-edgeo [n1 n2]
    (fn [sub]
      (let [edges (partition 2 mydata)
            n1 (walk sub n1)
            n2 (walk sub n2)
            ]
         (condp = (map lvar? [n1 n2])
           [true true]   (to-stream  (map (partial unify sub [n1 n2])  edges))
           [true false]  (to-stream  (map #(unify sub n1 (first %)) (filter (comp #{n2} second) edges)))
           [false true]  (to-stream  (map #(unify sub n2 (second %)) (filter (comp #{n1} first) edges)))
           [false false] (when ((set edges) [n1 n2]) sub)
           ))))


(defn patho [n1 n2]
  (conde
    [(my-edgeo n1 n2)]
    [(fresh [z]
            (my-edgeo n1 z)
            (patho z n2)
            )]))

(run 10 [q]
     (patho q 4))
