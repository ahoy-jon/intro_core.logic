(ns cdl.core
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic][clojure.core.logic.pldb])
  (:require [clojure.core.logic.fd :as fd]))





(def a 1)

(defn f [x] (inc x))

(f 1)


(run* [a b c]
      (== b c)
      (conde
       [(== c 1)]
       [(== c 2)]
      ))

(run* [q]
      (fresh [a b c]
             (fd/in a b c (fd/interval 0 100))
             (fd/+ a b c)
             (fd/* a b c)
             (== q [a b c])
             ))





(map vector [1 2 3 4] [:a :b :c :d])


(first
 (run 1 [a b]
      (zip° a b '([1 :a] [2 :b] [3 :c] [4 :d]))))


conso


(first

 (run 1 [a b c]

      (fresh [l ll]
             (== c ll)
             (conso 56 l ll)

      (fd/in a b (fd/interval 0 100))
      (map° #(fd/+ %1 1 %2)  [b a b] ll ))))



(run* [a]
   (fresh [result b]
      (== b 1)
      (== result {:b b})
      (== a      {:a result})))

(db-rel parento)

(def parentdb (db [parento 'Bart 'Homer]
                  [parento 'Homer 'Abraham]
                  [parento 'Abraham 'Plouf]))

(defn ancestoro [c a]
  (conde
   [(parento c a)]
   [(fresh [ap]
           (parento c ap)
           (ancestoro ap a)
           )]))

(with-db parentdb (run* [q]
                        (ancestoro 'Bart q)

                        ))






(defn disto [x d y]
  (conde
   [(== () d)(conde
             [(parento x y)]
             [(parento y x)]
             )]
   [(fresh [r z]
           (resto d r)
           (disto x () z)
           (disto z r  y)
           )]))




(defn inco [x y]
  (all
    (fd/in x y (fd/interval 0 100) )
    (fd/+ x 1 y)))

(defn disto [x d y]
  (conde
   [(== 0 d)
    (conde
      [(parento x y)]
      [(parento y x)])]
   [(fresh [r z]
           (inco r d)
           (disto x 0 z)
           (disto z r  y)
           )]
   )
  )

(with-db parentdb
  (run 10 [a b c]
      (== b 5)
      (disto a b c)))



(with-db parentdb (run* [a b] (ancestoro a b)))


(defmacro to-map [& body]
  `(zipmap (map keyword (quote ~body)) [~@body]))


(run 4 [a b q]
    ; (!= a b)
     (== a 1)
     (== q (to-map a b)))

(with-db parentdb (run* [c p] (ancestoro c p)))



(run* [q]
  (fresh [a b]
    (== 2 b)
    (conde
      [(== a b)]
      [(== a 1)])
    (== q {:result {:a a :b b}})))


(macroexpand '(to-map a))

(run 1 [q]
     (fresh [a b]
            (== q (to-map a b))
            (== a 1)
            (== b 2)))

(db-rel men m)
(db-rel robot m)
(db-rel language l d)

(def mydb (db
              [men 'Michel]
              [men 'Joe]
              [men 'Martin]
              [robot 'Borg]
              [language 'Martin 'Scala]
              [language 'Martin 'Java]
              [language 'Joe 'Erlang]
              [language 'Joe 'Prolog]
              [language 'Borg 'Zorg]))


(with-db mydb (run* [q a]
                   (men q)

                    (conde
                    [(language q a)]
                    [(== a nil)(nafc #(fresh [alla] (language % alla)) q)]

                    )))


(db-rel parent p c)

(def pdb (db
          [parent 'GpHomer 'Homer]
          [parent 'Homer 'Bart]
          ))


(run* [a b]
      (== a 1)
      (!= b a)
      )




(defn ancestor [x a]
  (conde
   [(parent a x)]
   [(fresh [y]
           (parent y x)
           (ancestor y a)
           )]
   ))

(with-db cdl.core/pdb
  (run 4 [a b]
       (ancestor a 'GpHomer)
       ))

(with-db mydb

  (run* [e l] (language e l))
  )


(defn lm [e men? lang]
        (conde
         [          (language e lang)             (men e)(== men? true)]
         [          (language e lang)             (nafc men e)(== men? false)]
         [(nafc #(fresh [l] (language % l)) e)    (men e)(== men? true)]))

(with-db mydb
  (run* [ e b l] (lm e b l)))

 (defn cts [e men? lang]
        (fresh [m l]
               (!= e nil)
               (conde
                [(== m e)(== m l)]
                [(== l e)(== m nil)]
                [(== m e)(== l nil)])
               (conde
                [(language l lang)]
                [(== l nil)(nafc #(fresh [x] (language % x)) m)])
               (conde
                [(men m)(== men? true)]
                [(== m nil)(nafc men l)(== men? false)])))


 (set (with-db mydb (run* [e]
                     (fresh [l]
                            (language e l)))))


 (with-db mydb (run* [e men? lang]
                     (cts e men? lang)
                     ))




;; https://github.com/Djebbz/core-logic-primer


;; BEGINNING SECRET SANTA

(def data
   (partition 2 '[Walter White
                  Skyler White
                  Gustavo Fring
                  Saul Goodman
                  Jesse Pinkman
                  Henry Schrader
                  Marie Schrader]))


;; like clojure.core/map, but relationnal
(defn map° [g & ls]
  (conde
   [(everyg (partial == '()) ls)]
   [(let [n (count ls)
          heads (repeatedly n lvar)
          tails (repeatedly n lvar)]
      (all (everyg (partial apply conso)(map vector heads tails ls))
           (apply g heads)
           (apply map° g tails)))]))

;; we can implement a more generic version of everyg with map°
(defn everyg° [g col] (map° g col))

; or ; (def everyg° map°)


;; like clojure.core/vector, but relationnal
(defn vector° [& args]
  (== (vec (butlast args)) (last args)))

;; like (map vector a b), but relationnal
(defn zip° [a b c]
  (map° vector° a b c))


(run 2 [a b]
  (zip° a b (list [1 :a] [2 :b] [3 :c])))


;; the only rule of the problem, checking that a couple is correct.
(defne couple° [z]
  ;; Lastnames must be different
  ([[[_ x][_ y]]] (!= x y)))


(run 1 [couples]
     (fresh [giveto]
            (zip° data giveto couples)
            (permuteo data giveto)
            (everyg° couple° couples)))

#_ '(([(Walter White)   (Gustavo Fring) ]
      [(Skyler White)   (Saul Goodman)  ]
      [(Gustavo Fring)  (Jesse Pinkman) ]
      [(Saul Goodman)   (Henry Schrader)]
      [(Jesse Pinkman)  (Marie Schrader)]
      [(Henry Schrader) (Walter White)  ]
      [(Marie Schrader) (Skyler White)  ]))

;;; END SECRET SANTA





(defne match-map [m o]
  ([{:foo {:bar o}} _]))

(run* [q]
     (fresh [a b]
     (== a {:foo {:bar :quxx}})
     (== [a b] q)
     (match-map a b)))


(run 1 [a]
      (permuteo data a))


(def deps {:d [:c :b] :b [:a :0] :c [:a] :e [:d :c] :a [] :0 []})

(def task-time {:a 10 :b 2 :c 5 :d 10 :0 15})

(defne offseto [l]
  ([[_ o]] (fd/in o (fd/interval 0 100))))


(defn depso [k ds]
  (membero [k ds] (vec deps)))

(defn printlno [& rest]
  (== nil (do (apply println rest)
            (flush))))

(run* [tt]
       (== tt (map vector (keys task-time) (repeatedly lvar)))
       (everyg offseto tt)
       (everyg (fn [t]
                 (fresh [k l ds]
                        (== t [k l])
                        (depso k ds)

                        (conde [(== ds [])(== l 0)]
                               [(everyg (fn [d]
                                  (fresh [l2 ttime l2end]
                                         (membero [d l2] tt)
                                         (membero [d ttime] (vec task-time))
                                         (fd/+ l2 ttime l2end)
                                         (fd/>= l l2end))) ds)
                               (fresh [d l2 ttime]
                                      (membero d ds)
                                      (membero [d l2] tt)
                                      (membero [d ttime] (vec task-time))
                                      (fd/+ l2 ttime l))]
                               )

                        )

                 ) tt)



       )


