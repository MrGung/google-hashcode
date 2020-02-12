(defproject hashcode "0.0.1-SNAPSHOT"
  :description "FIXME: write description"
  :dependencies [[org.clojure/clojure "1.10.1"]

                 [clj-commons/pomegranate "1.2.0"] ;; dynamisches Laden von Libs: https://github.com/clj-commons/pomegranate
;                 => (use '[cemerick.pomegranate :only (add-dependencies)])
;                 nil
;                 => (add-dependencies :coordinates '[[incanter "1.9.2"]]
;                                      :repositories (merge cemerick.pomegranate.aether/maven-central
;                                                           {"clojars" "https://clojars.org/repo"}))
;                 ;...add-dependencies returns full dependency graph...
;                 => (require '(incanter core stats charts))

                 ]
  :aot [hashcode2018.core]
  :main hashcode2018.core)
