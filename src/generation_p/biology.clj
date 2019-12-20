(ns generation-p.biology
  (:require [clojure.data.generators :as data.gen]
            [generation-p.model :as m]
            [generation-p.social :as social]
            [generation-p.image :as image]))

(def ^:const desired-generation-population-count 60)
(def ^:const crossover-mutation-rate 0.02)

;; width and height are desired dimensions of resultant image -- the return val
;; of this function is a 1D vector of size (* height width num-channels)
(defn- random-chromosome
  ([]
   (random-chromosome image/img-width))
  ([width]
   (-> width
       (Math/pow 2)
       (take (repeatedly #(apply data.gen/one-of image/palette)))
       flatten)))

(defn spawn-random-individual []
  {::m/id               (java.util.UUID/randomUUID)
   ::m/social-id        nil
   ::m/generation-num   0
   ::m/chromosome       (random-chromosome)
   ::m/parent0-id       nil
   ::m/parent1-id       nil
   ::m/crossover-method nil
   ::m/crossover-params nil})

;; ==============================================================================
;; Selection
;; ==============================================================================

(defn- normalize-fitness [population]
  (let [fitness-sum (reduce (fn [acc el] (+ (:fitness el) acc))
                            0
                            population)]
    (pmap #(assoc %
                  :norm-fitness
                  (/
                   (:fitness %)
                   fitness-sum))
          population)))

(defn- accumulate-normalized-fitnesses [population]
  (reduce (fn [acc el]
            (if (empty? acc)
              [(assoc el :acc-norm-fitness (:norm-fitness el))]
              (conj acc (assoc el :acc-norm-fitness
                               (+ (:norm-fitness el)
                                  (:acc-norm-fitness (last acc)))))))
          []
          population))

(defn- choose-parent-from-population
  ([population]
   (choose-parent-from-population population (data.gen/float)))
  ([population cutoff]
   ;; individuals in `population` should already have an accumulated normalized
   ;; fitness and be sorted in ascending order
   (->> population
        (filter #(>= (:acc-norm-fitness %) cutoff))
        first)))

(defn matchmake [population]
  ;; Fitness proportionate selection
  ;; Given a population, choose two individuals to reproduce.
  ;; https://en.wikipedia.org/wiki/Selection_(genetic_algorithm)
  (let [accumulated-normalized-fitnesses
        (->> population
             ;; The fitness function is evaluated for each individual, providing
             ;; fitness values, which are then normalized.
             (mapv #(assoc % :fitness (social/get-fitness %)))
             normalize-fitness
             ;; The population is sorted by ascending fitness values.
             (sort-by :norm-fitness)
             ;; Accumulated normalized fitness values are computed: the
             ;; accumulated fitness value of an individual is the sum of its own
             ;; fitness value plus the fitness values of all the previous
             ;; individuals
             accumulate-normalized-fitnesses)
        ;; A random number R between 0 and 1 is chosen
        ;; The selected individual is the first one whose accumulated normalized
        ;; value is greater than or equal to R.
        choose-parent #(choose-parent-from-population
                        accumulated-normalized-fitnesses)
        parent0       (choose-parent)
        ;; Ensure parent1 is different from parent0. One can't reproduce with
        ;; oneself, unfortunately.
        parent1       (loop [p (choose-parent)]
                        (if (= (::m/id p) (::m/id parent0))
                          (recur (choose-parent))
                          p))]
    [parent0 parent1]))

;; ==============================================================================
;; Crossover
;; ==============================================================================

;; like k-point crossover, but instead of k points, (randomly) crossover after
;; runs of length n
(defn- crossover [{:keys [n]} parent0 parent1]
  (let [parent0-partitioned (partition (* n image/num-channels) parent0)
        parent1-partitioned (partition (* n image/num-channels) parent1)]
    (flatten
     (for [i (range (count parent0-partitioned))]
       (if (data.gen/boolean)
         (nth parent0-partitioned i)
         (nth parent1-partitioned i))))))

;; get the coords of n patches that evenly cover a width x height 2D array,
;; taking image/num-channels into account
;; e.g.
;;
;; an image that is 2x2 will have a width of 6 (2 pixels * 3 color channels)
;;
;; (patches 1 6 2)
;;
;; (([0 0] [0 1] [0 2])  ;; first patch is nw pixel
;;  ([0 3] [0 4] [0 5])  ;; second patch is ne pixel
;;  ([1 0] [1 1] [1 2])  ;; third patch is sw pixel
;;  ([1 3] [1 4] [1 5])) ;; fourth patch is se pixel
(defn- patches [n width height]
  (for [patch-indices-i (partition n (range height))
        patch-indices-j (partition (* n image/num-channels) (range width))]
    (for [i patch-indices-i
          j patch-indices-j]
      [i j])))

(def patches-memo (memoize patches))

(defn- reshape [width flattened-arr]
  (->> flattened-arr
       (partition width)
       (pmap vec)
       vec))

;; TODO refactor
;; This is a specialized crossover approach, which treats 2D patches of size nxn
;; as genes in the chromosome/individual. Given two parents, these patches are
;; randomly exchanged to form a new individual.

;; e.g., if n=1 and each digit in the grid represents a pixel in the image

;; parent0
;; 0 0 0
;; 0 0 0
;; 0 0 0

;; parent1
;; 1 1 1
;; 1 1 1
;; 1 1 1

;; possible child
;; 0 1 0
;; 1 1 0
;; 1 0 1

(defn patch-crossover
  [{:keys [n]} parent0 parent1]
  ;; loop over the reshaped vector, patch by patch, taking a given patch from
  ;; either parent randomly
  ;; note: vector dimensions ideally are evenly divided by n
  ;; note2: assumes square image
  (let [width      (-> parent0
                       count
                       (/ image/num-channels)
                       Math/sqrt
                       (* image/num-channels)
                       int)
        parent0-2d (reshape width parent0)
        parent1-2d (reshape width parent1)]
    (flatten
     (loop [[patch & rest-patches] (patches-memo n
                                                 (count (first parent0-2d))
                                                 (count parent0-2d))
            img                    parent0-2d]
       (if patch
         (recur
          rest-patches
          (let [src-img (data.gen/one-of parent0-2d parent1-2d)]
            (reduce
             (fn [res-img pixel]
               (->> (get-in src-img pixel)
                    (assoc-in res-img pixel)))
             img
             patch)))
         img)))))

(defn- random-crossover-method []
  (data.gen/one-of ::m/crossover ::m/patch-crossover))

(defn- crossover-n-vals []
  (let [vec-width      (-> image/img-width
                           (Math/pow 2)
                           int)
        half-vec-width (int (/ vec-width 2))]
    (->> (range 1 (inc half-vec-width))
         (filter #(zero? (mod vec-width %))))))

(def ^:private crossover-n-vals-memo (memoize crossover-n-vals))

(defn- patch-crossover-n-vals []
  (let [half-width (/ image/img-width 2)]
    (->> (range 1 (inc half-width))
         (filter #(zero? (mod image/img-width %))))))

(def ^:private patch-crossover-n-vals-memo (memoize patch-crossover-n-vals))

(defn- random-crossover-params [crossover-method]
  (case crossover-method
    ::m/crossover
    {:n (apply data.gen/one-of (crossover-n-vals-memo))}
    ::m/patch-crossover
    {:n (apply data.gen/one-of (patch-crossover-n-vals-memo))}))

(defn- crossover-method->fn [crossover-method crossover-params]
  (case crossover-method
    ::m/crossover
    (partial crossover crossover-params)

    ::m/patch-crossover
    (partial patch-crossover crossover-params)))

(defn breed [parent0 parent1]
  ;; nsfw
  (let [crossover-parent  (data.gen/one-of parent0 parent1)
        mutate-crossover? (< (data.gen/float) crossover-mutation-rate)
        crossover-method  (or
                           (and (not mutate-crossover?)
                                (::m/crossover-method crossover-parent))
                           (random-crossover-method))
        crossover-params  (or
                           (and (not mutate-crossover?)
                                (::m/crossover-params crossover-parent))
                           (random-crossover-params crossover-method))
        crossover-fn      (crossover-method->fn crossover-method crossover-params)]
    (merge (spawn-random-individual)
           {::m/generation-num (inc (::m/generation-num parent0))
            ::m/chromosome (crossover-fn (::m/chromosome parent0)
                                         (::m/chromosome parent1))
            ::m/parent0-id (::m/id parent0)
            ::m/parent1-id (::m/id parent1)
            ::m/crossover-method crossover-method
            ::m/crossover-params crossover-params})))
