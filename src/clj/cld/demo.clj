(ns cld.demo
  (:refer-clojure :exclude [==])
  (:require [clojure.core.logic.fd :as fd]
            [clojure.pprint :as pp]
            [clojure.set :as s]
            [clojure.math.combinatorics :as combo]
            [clojure.tools.macro :as macro])
  (:use [clojure.core.logic]))



;; VENN DEMO

(def some-data
  {:a (range 10)
   :b (range 0 10 2)
   :c (range 0 10 3)})





(defn combinaison [xs]
  "every subsets, except ()"
  (->> (combo/subsets xs)
       (filter (comp not empty?))
       (map set)
       ))






(defn calc [data combi]
  (let [all (set (map first data))
        others (s/difference all combi)
        mix-sets (fn [f k] (reduce f (map (comp set data) k)))
        others-data (mix-sets s/union others)
        inter-data (mix-sets s/intersection combi)]
         (s/difference inter-data others-data)))


(calc some-data  [:a :b :c])


(into {} (filter (comp not empty? second)  (map
 (juxt identity (partial calc some-data))
 (combinaison (map first some-data)))))



({#{:a}       #{1 5 7},
  #{:a :b}    #{2 4 8},
  #{:a :c}    #{3 9},
  #{:a :c :b} #{0 6}}
              #{:a :b :c})

(def print #'pp/pprint)


#_ (reduce str "ahoy" (range 10))




(defrel column server tablename name)

(defrel needtable who tablename)

(defrel excludeCol tablename name)

(fact excludeCol "Member" "Password")


(fact column :1 "Member" "FirstName")
(fact column :1 "Member" "LastName")
(fact column :1 "Member" "Password")

(fact column :1 "Profil" "Content")

(fact needtable :BI "Member")

(defrel colNotNeeded who tablename column)


(fact colNotNeeded :BI "Member" "LastName")

(fact needtable :DMML "Member")



(distinct  (run* [q] (fresh [s t c w ct] (column s t c)
                 (needtable w t)
                 (nafc colNotNeeded w t c)
                 (nafc excludeCol t c)
                 (project [t c] (== ct (str t "." c)))
                 (== {:table t :column c :info {:plus ct}} q)))
)




(run* [q] (== q 1))

(print {:a :b})



(defne righto [x y l]
  ([_ _ [x y . ?r]])
  ([_ _ [_ . ?r]] (righto x y ?r)))

(defn nexto [x y l]
  (conde
    ((righto x y l))
    ((righto y x l))))

(defn zebrao [hs]
  (macro/symbol-macrolet [_ (lvar)]
    (all
     (== [_ _ [_ _ 'milk _ _] _ _] hs)
     (firsto hs ['norwegian _ _ _ _])
     (nexto ['norwegian _ _ _ _] [_ _ _ _ 'blue] hs)
     (righto [_ _ _ _ 'ivory] [_ _ _ _ 'green] hs)
     (membero ['englishman _ _ _ 'red] hs)
     (membero [_ 'kools _ _ 'yellow] hs)
     (membero ['spaniard _ _ 'dog _] hs)
     (membero [_ _ 'coffee _ 'green] hs)
     (membero ['ukrainian _ 'tea _ _] hs)
     (membero [_ 'lucky-strikes 'oj _ _] hs)
     (membero ['japanese 'parliaments _ _ _] hs)
     (membero [_ 'oldgolds _ 'snails _] hs)
     (nexto [_ _ _ 'horse _] [_ 'kools _ _ _] hs)
     (nexto [_ _ _ 'fox _] [_ 'chesterfields _ _ _] hs))))


(run* [hs] (zebrao hs))
