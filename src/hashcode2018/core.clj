(ns hashcode2018.core
  (:gen-class)
  (:require [clojure.string :as s]))


(defn -main []
  (println "Hello, World!"))



;; ## parse input
(defn parse-line-to-integers [line]
  (-> line
      (s/split #" ")
      ((partial map #(Integer/parseInt %)))))

(defn parse-header [line]
  (->> line
       parse-line-to-integers
       (zipmap [:#rows :#columns :#vehicles :#rides :bonus :ticks])))

(defn parse-ride [line]
  (->> line
       parse-line-to-integers
       (zipmap
        [:#row-start :#column-start :#row-finish :#column-finish :#earliest-start :#latest-finish])))

(defn parse-input [filename]
  (with-open [rdr (clojure.java.io/reader
                   (.getPath (clojure.java.io/resource filename)))]
    (let [lines  (line-seq rdr)
          header (parse-header (first lines))
          rides  (doall (map parse-ride (rest lines)))]
      {:header header
       :rides  rides})))


;; ## build structures
(defn assign-rides-to-grid [rides]
  (reduce
   (fn [grid ride]
     (let [coord [((juxt :#row-start :#column-start) ride)]]
       (println coord)
       (update-in grid
                  coord
                  conj
                  ride)))
   {}
   rides))




(let [{:keys [header rides]} (parse-input "hashcode2018/a_example.in")]
  ;  (clojure.pprint/pprint header)
  ;  (clojure.pprint/pprint rides)
  (let [grid (assign-rides-to-grid rides)]
    (clojure.pprint/pprint grid)))
