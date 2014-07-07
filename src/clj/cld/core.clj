(ns cdl.core
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic][clojure.core.logic.pldb])
  (:require [clojure.core.logic.fd :as fd])
  )


;;; var a = 1;
;;   (fn [x y] (+ x y))



(def a 1)

(run* [a]
      (fresh [result b]
      (== result {:b b, :c 2})
      (== a  {:a result})))

(db-rel parento)

(def parentdb (db [parento 'Bart 'Homer]
                  [parento 'Homer 'Abraham]
                  [parento 'Abraham 'Plouf]
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
           )]
   )

  )



(defn tableo [who when table])

(defn not-Allowedo [table col])

(defn columno [server table col])


(defn ancestoro [c a]
  (conde
   [(parento c a)]
   [(fresh [ap]
           (parento c ap)
           (ancestoro ap a)
           )]))


(with-db parentdb (run* [a b] (ancestoro a b)))



(def a 1 )
(def b 2)

(to-map a b)

(run 4 [a b q]
     (!= a b )
     (== a 1)
     (== q (to-map a b)))

(with-db parentdb (run* [c p]
      (ancestoro c p)))



(run* [q]
      (fresh [a b]
             (== 2 b)
             (conde
              [(== a b)]
              [(== a 1)])
             (== q {:result  {:a a :b b}})
             ))




(defmacro to-map [& body]
  `(zipmap (map keyword (quote ~body)) [~@body]))

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
  (run*  [ e b l]
                (lm e b l)))

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

 (def data
   (partition 2 '[Walter White
                  Skyler White
                  Gustavo Fring
                  Saul Goodman
                  Jesse Pinkman
                  Henry Schrader
                  Marie Schrader]))

(defne oreo
  "MMmmm COOOKIES" ;; I don't know how to name that one
  [x y z]
  ([a a a])
  ([a b a] (!= a b))
  ([b a a] (!= a b)))

(defne zipo
  "zipped = (map (partial vector) xl yl)"
  [xl yl zipped]
  ([xs ys ()] (membero () [xs ys]))
  ([[x . xs] [y . ys] [z . zs]]
     (== z [x y])
     (zipo xs ys zs)))

(defne ruleo [z]
  ([()])
  ([[[[_ x] [_ y]] . as]] ; extract names of the first pair
   (!= x y)
   (ruleo as)))

(run 10  [couple]
     (fresh [result]
            (zipo data result couple)
            (permuteo data result)
            (ruleo couple)))

'({:z ([(Walter White) (Walter White)] [(Skyler White) (Skyler White)]
       [(Gustavo Fring) (Gustavo Fring)] [(Saul Goodman) (Saul Goodman)]
       [(Jesse Pinkman) (Jesse Pinkman)] [(Henry Schrader) (Henry Schrader)]
       [(Marie Schrader) (Marie Schrader)]),
   :l2 ((Walter White) (Skyler White) (Gustavo Fring) (Saul Goodman) (Jesse Pinkman)
        (Henry Schrader) (Marie Schrader)),
   :l1 ((Walter White) (Skyler White) (Gustavo Fring)
        (Saul Goodman) (Jesse Pinkman) (Henry Schrader) (Marie Schrader))})




 (defne match-map [m o]
  ([{:foo {:bar o}} _]))


 (run 1 [a]
      (permuteo data a)
   )




