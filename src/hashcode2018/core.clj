(ns hashcode2018.core
  (:gen-class)
  (:require [clojure.string :as s]
            [clojure.pprint :as pp]
            [taoensso.timbre :as l]                         ; https://github.com/ptaoussanis/timbre
    ;; (l/spy :info (* 5 4 3 2 1)) => 120
    ;; (l/info "lexical env: " (l/get-env)) = lexical info!
            ))




;; ## helper
;; ### display
(defn p
  ([obj] (p nil obj))
  ([msg obj]
   (if msg (print msg))
   (pp/pprint obj)))

(defn l [msg obj]
  (println msg obj))


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
    (let [lines (line-seq rdr)
          header (parse-header (first lines))
          rides (doall (map parse-ride (rest lines)))]
      {:header header
       :rides  rides})))


;; ## build structures
(defn assign-rides-to-grid [rides]
  (reduce
    (fn [grid ride]
      (let [coord [((juxt :#row-start :#column-start) ride)]]
        (update-in grid
                   coord
                   conj
                   ride)))
    {}
    rides))

(defn create-cars [amount]
  (for [car-id (range amount)]
    {:car-id car-id
     :rides  []
     :tick   0}))


;; ## magic
;; ### position and distance
(defn get-start-position-ride [ride]
  ((juxt :#row-start :#column-start) ride))

(defn get-finish-position-ride [ride]
  ((juxt :#row-finish :#column-finish) ride))

(defn get-position [car]
  (or
    (some-> car
            :rides
            last
            get-finish-position-ride)
    [0 0]))

(defn dist [[r1 c1] [r2 c2]]
  (Math/abs (+ (- r1 r2) (- c1 c2))))


;; ### sorting

(defn sort-grid-by-distance [car-pos grid]
  (sort-by (comp (partial dist car-pos) key) grid))

(defn scan-grid-for-next-ride [car-pos grid]
  (if-let [ride (some->
                  (get grid car-pos)
                  first)]
    (do
      (l/debug "found ride on car's position")
      ride)
    (let [sorted-grid (sort-grid-by-distance car-pos grid)]
      (-> sorted-grid
          first
          first))))

;; ### working with the grid
(defn gc [grid ride-pos]
  (cond-> grid
          (empty? (get grid ride-pos)) (dissoc ride-pos)))

(defn find-next-ride [car grid]
  (if-let [next-ride (scan-grid-for-next-ride (get-position car) grid)]
    (let [next-ride-start-pos (get-start-position-ride next-ride)
          next-ride-finish-pos (get-finish-position-ride next-ride)
          updated-grid (l/spy :debug "grid"
                              (-> grid
                                  (update next-ride-start-pos (partial remove #{next-ride}))
                                  (gc next-ride-start-pos)))]
      (l/debug (format "next ride going from %s to %s" next-ride-start-pos next-ride-finish-pos))
      {:ride next-ride :updated-grid updated-grid})
    (do
      (l/debug "Found no ride")
      nil)))

(defn assign-rides
  "Weißt den Cars die Rides in einer Runde zu."
  [cars grid]
  (reduce (fn [{:keys [new-grid] :as result} car]
            (if (not (empty? new-grid))
              (do
                (l/debug "grid is not empty")
                (if-let [{:keys [ride updated-grid]} (find-next-ride car grid)]
                  (do
                    (l/debug (format "found next ride: %s \nfor car: %s" ride car))
                    (let [ticks-to-add-for-car (l/spy :debug "ticks for ride" (dist (get-position car) (get-finish-position-ride ride)))
                          updated-car (l/spy :debug "updated-car: " (-> car
                                                                        (update :rides conj ride)
                                                                        (update :tick + ticks-to-add-for-car)))
                          new-result (-> result
                                         (update :new-cars conj updated-car)
                                         (assoc :new-grid updated-grid))]
                      (l/spy :debug "new result" new-result)))
                  (do (l/warn "no ride round!" grid)        ;; unschön: könnte mehrmals ausgegeben werden
                      result)))
              (do
                (l/info "no more rides to assign")
                result                                      ;; bisheriges Ergebnis zurückgeben.
                )))
          {:new-cars [] :new-grid grid}
          cars))


;; ## driver

(defn configure-logging []
  (comment (l/println-appender)
           (taoensso.timbre.appenders.core/println-appender))

  (l/set-config! #_{
                    :timestamp-opts {}
                    :level          :debug
                    }
    (assoc l/example-config
      :timestamp-opts {})))


(defn -main []
  (configure-logging)
  (let [{:keys [header rides]} (parse-input "hashcode2018/a_example.in")]
    (l "header parsed in input: " header)
    (l "number of rides in input: " (count rides))
    (clojure.pprint/print-table rides)
    (let [grid (assign-rides-to-grid rides)
          cars (create-cars (:#vehicles header))
          ;;(p cars)
          ;;(p grid)
          ]
      ;; für später
      (let [{:keys [new-cars new-grid]} (assign-rides cars grid)]
        (p "cars: " new-cars)
        )


      ;;(let [car (first cars)
      ;;      car-pos (get-position car)]
      ;;  (p "car: " car)
      ;;  (p "car-pos: " car-pos)
      ;;  (let [{:keys [ride grid]} (find-next-ride car-pos grid)]
      ;;    (p "next-ride: " ride)
      ;;    (p "resulting in grid: " grid)))
      )
    (println "fertig")))

(comment

  )
