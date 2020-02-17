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

;; #### logging

(defn configure-logging []
  (l/set-config! l/example-config)
  #_(l/merge-config! {:appenders
                      {:println
                       (taoensso.timbre.appenders.example/example-appender)
                       ;;{:output-fn (fn [{:keys [msg_]}] (force msg_))}
                       }}))


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

;; ### position and distance
(defn get-start-position-ride [ride]
  (:start-pos ride))

(defn get-finish-position-ride [ride]
  (:finish-pos ride))

(defn get-position [car]
  (or
    (some-> car
            :rides
            last
            get-finish-position-ride)
    [0 0]))

(defn dist [[r1 c1] [r2 c2]]
  (Math/abs (+ (- r1 r2) (- c1 c2))))


;; ### augment data

(defn augment-ride
  "Augments rides with additional data. Returns nil if ride cannot be done (is invalid)."
  [ride]
  (let [start-pos ((juxt :#row-start :#column-start) ride)
        finish-pos ((juxt :#row-finish :#column-finish) ride)
        dist (dist start-pos finish-pos)
        ]
    (if (> dist (- (:#latest-finish ride)
                   (:#earliest-start ride)))
      (do
        (l/warn "Ride cannot be done: " ride)
        nil                                                 ;; the ride cannot be done
        )
      (assoc ride :dist dist
                  :start-pos start-pos
                  :finish-pos finish-pos))))

;; ### driving the parsing
(defn parse-input [filename]
  (with-open [rdr (clojure.java.io/reader
                    (.getPath (clojure.java.io/resource filename)))]
    (let [lines (line-seq rdr)
          header (parse-header (first lines))
          rides (doall (->> lines
                            rest
                            (map (comp augment-ride parse-ride))
                            (remove nil?)))]
      {:header header
       :rides  rides})))


;; ## build structures
(defn assign-rides-to-grid
  "Places rides on their starting-positions in the grid."
  [rides]
  (reduce
    (fn [grid ride]
      (let [coord (:start-pos ride)]
        (update grid
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
;; ### sorting

(defn sort-grid-by-distance [car-pos grid]
  (sort-by (comp (partial dist car-pos) first) grid))

(defn scan-grid-for-next-ride
  "Scans the grid for a next ride - takes the closest ride to a car's position."
  [car-pos grid]
  (let [sorted-grid (sort-grid-by-distance car-pos grid)
        impossible-ride? (fn [ride]
                           (< (:#latest-finish ride)
                              (+ (dist car-pos (:start-pos ride))
                                 (:dist ride))))
        ;; remove rides than cannot be performed by this car from its current position
        filtered-grid (reduce (fn [grid [coord rides]]
                                (let [filtered-rides (remove impossible-ride? rides)]
                                  (if (seq filtered-rides)
                                    (assoc grid coord filtered-rides)
                                    grid)))
                              {}
                              sorted-grid)]
    ;; take the first ride from the nearest location in the grid
    (l/spy :debug "ride from filtered-grid" (->> filtered-grid
                                                 first
                                                 second
                                                 first))))

;; ### working with the grid
;; #### clean up
(defn gc
  "Garbage-collects the grid: remove coords that no longer have rides associated."
  [grid ride-pos]
  (cond-> grid
          (empty? (get grid ride-pos)) (dissoc ride-pos)))

(defn gc-full-grid
  "Garbage-collect all rides that can no longer be done by any car."
  [cars grid]
  (let [min-tick (l/spy :debug "min-tick" (->> cars
                                               (map :tick)
                                               (reduce min)))]
    (reduce (fn [m [coord rides]]
              (let [filtered-rides (filter #(> min-tick %) rides)]
                (if (seq filtered-rides)
                  (assoc m coord filtered-rides)
                  m)))
            {}
            grid)))


;; #### searching and finding rides
(defn find-next-ride
  "Find the car's next ride - remove that ride from the grid."
  [car grid]
  (if-let [next-ride (l/spy :debug "next-ride from find-next-ride: " (scan-grid-for-next-ride (get-position car) grid))]
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
  "Assigns rides to cars - cars are iterated once."
  [cars grid]
  (reduce (fn [{:keys [new-grid] :as result} car]
            (if (not (empty? new-grid))
              (do
                (l/debug "grid is not empty")
                (if-let [{:keys [ride updated-grid]} (find-next-ride car grid)]
                  (do
                    (let [new-tick-of-car (l/spy :debug "ticks for ride"
                                                 (let [getting-there (dist (get-position car) (get-start-position-ride ride))
                                                       tick (:tick car)
                                                       new-tick-of-car (+ (max (+ tick getting-there)
                                                                               (:#earliest-start ride))
                                                                          (:dist ride))]
                                                   new-tick-of-car))
                          updated-car (l/spy :debug "updated-car: " (-> car
                                                                        (update :rides conj ride)
                                                                        (assoc :tick new-tick-of-car)))
                          new-result (-> result
                                         (update :new-cars conj updated-car)
                                         (assoc :new-grid updated-grid))]
                      (l/spy :debug "new result" new-result)))
                  (do (l/warn "no ride round!" grid)        ;; unschön: könnte mehrmals ausgegeben werden
                      result)))
              (do
                (l/info "no more rides to assign")
                (reduced result)                            ;; bisheriges Ergebnis zurückgeben.
                )))
          {:new-cars [] :new-grid grid}
          cars))


(defn assign-all-rides [cars grid]
  (if (empty? grid)
    cars
    (let [{:keys [new-cars new-grid]} (assign-rides cars grid)
          new-grid (gc-full-grid new-cars new-grid)]
      (recur new-cars new-grid))))

;; ## driver




(defn -main []
  (configure-logging)
  (let [{:keys [header rides]} (parse-input "hashcode2018/a_example.in")]
    (l "header parsed in input: " header)
    (l "number of rides in input: " (count rides))
    (clojure.pprint/print-table rides)
    (let [grid (assign-rides-to-grid rides)
          cars (create-cars (:#vehicles header))]
      (let [new-cars (assign-all-rides cars grid)]
        (p "cars: " new-cars))


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
