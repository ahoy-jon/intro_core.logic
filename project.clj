(defproject core.logic.demo "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [
                 ;; the stuff
                 [org.clojure/clojure "1.5.1"]
                 [org.clojure/core.logic "0.8.5"]

                 ;; to host the prez
                 [ring/ring-jetty-adapter "1.2.1"]
                 [compojure "1.2.0-SNAPSHOT"]

                 ]


  :source-paths ["src/clj"])
