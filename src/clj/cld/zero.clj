
(ns zero)


;; THIS CODE DECRIBE HOW TO IMPLEMENT YOUR OWN LOGIC ENGINE


(defn lvar
  ([] (lvar ""))
  ([nm] (gensym (str nm "_"))))

(def lvar? symbol?)

(defn walk [s u]
    (if-let [pr (get s u)]
      (if (lvar? pr) (recur s pr) pr)
      u))

(defn unify [s u v]
  (let [u (walk s u)
        v (walk s v)]
    (cond (and (lvar? u) (lvar? v) (= u v)) s
          (lvar? u) (assoc s u v)
          (lvar? v) (assoc s v u)
          :else (and (= u v) s)

    )
  ))

(unify {} (lvar) 1)



(defn === [a b]
  (fn [s]
    (if-let [v (unify s a b)]
      [v]
      [])))


((=== (lvar) 1) {})

(defn -conj ([a] a)
  ([a b] (fn [s]
           (for [aret (a s)
                 :when aret
                 bret (b aret)
                 :when bret
                 ]
             bret)))
  ([a b & more]
   (-conj a (apply -conj b more))))


((let [a (lvar "a")
       b (lvar "b")]
  (-conj (=== a b) (=== a 1) (=== b 2))) {})

(defn -disj [& goals]
  (fn [s]
    (mapcat (fn [goal] (goal s)) goals)
    ))

(let [a (lvar)]
((-disj
 (=== a 1)
 (=== a 2)
 ) {}))


(defn conde [& clauses]
  (apply -disj (map (partial apply -conj) clauses)))


((let [[a b] [(lvar "a") (lvar "b")]] (conde [(=== a 1) (=== a b)] [(=== a 2)])) {})








