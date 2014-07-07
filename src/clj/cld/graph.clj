(ns cdl.graph
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic][clojure.core.logic.pldb])
  (:require [clojure.core.logic.fd :as fd]))

(db-rel node index entity properties)

(db-rel relation start end type properties)

(def graphdb (db
              [node 'Movie 'matrix1 {:id 603 :title "The Matrix" :year "1999-03-31"}]
              [node 'Movie 'matrix2 {:id 604 :title "The Matrix Reloaded" :year "2003-05-07"}]
              [node 'Actor 'neo  {:name "Keanu Reeves"}]
              [node 'Actor 'morpheus {:name "Laurence Fishburne"}]
              [node 'Actor 'trinity {:name "Carrie-Anne Moss"}]
              [relation 'neo 'matrix1 :ACTS_IN {:role "Neo"}]
              [relation 'neo 'matrix2 :ACTS_IN {:role "Neo"}]
              [relation 'neo 'matrix3 :ACTS_IN {:role "Neo"}]
              [relation 'morpheus 'matrix1 :ACTS_IN {:role "morpheus"}]
              [relation 'morpheus 'matrix2 :ACTS_IN {:role "morpheus"}]
              [relation 'morpheus 'matrix3 :ACTS_IN {:role "morpheus"}]
              [relation 'trinity 'matrix1 :ACTS_IN {:role "trinity"}]
              [relation 'trinity 'matrix2 :ACTS_IN {:role "trinity"}]
              [relation 'trinity 'matrix3 :ACTS_IN {:role "trinity"}]))

(def _ lvar)

(with-db graphdb (run* [t n]
                       (fresh [a m ca cm]
                              (node (_) a ca)
                              (node (_) m cm)
                              (relation a m :ACTS_IN (_))
                              (== ca  (partial-map {:name n}))
                              (==  cm (partial-map {:title t})))))

'(["The Matrix Reloaded" "Keanu Reeves"]
  ["The Matrix" "Keanu Reeves"]
  ["The Matrix Reloaded" "Laurence Fishburne"]
  ["The Matrix" "Laurence Fishburne"]
  ["The Matrix Reloaded" "Carrie-Anne Moss"]
  ["The Matrix" "Carrie-Anne Moss"])




#_ (db-rel triple entity attribute value)
(db-rel tripple-properties entity properties)
(db-rel tripple-eav entity attribute value)
(def tripledb
  (db
              [tripple-properties 'matrix1  {:type 'Movie :id 603 :title "The Matrix" :year "1999-03-31"}]
              [tripple-properties 'matrix2  {:type 'Movie :id 604 :title "The Matrix Reloaded" :year "2003-05-07"}]
              [tripple-properties 'neo      {:type 'Actor :name "Keanu Reeves"}]
              [tripple-properties 'morpheus {:type 'Actor :name "Laurence Fishburne"}]
              [tripple-properties 'trinity  {:type 'Actor :name "Carrie-Anne Moss"}]
              [tripple-eav        'neo      {:role "Neo" :type :ACTS_IN} 'matrix1 ]
              [tripple-eav        'neo      {:role "Neo" :type :ACTS_IN} 'matrix2 ]
              [tripple-eav        'neo      {:role "Neo" :type :ACTS_IN} 'matrix3 ]))


(run* [k v]
      (membero [k v] (vec  {:type 1}) )
      )

(defn triple [e a v]
  (conde
     ((tripple-eav e a v))
     ()
   )
  )
