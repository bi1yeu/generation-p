(ns generation-p.core
  (:gen-class)
  (:require [clojure.data.generators :as data.gen]
            [generation-p.model :as m]
            [generation-p.social :as social])
  (:import
   (java.io File)
   (java.awt.image AffineTransformOp BufferedImage WritableRaster)
   (java.awt.geom AffineTransform)
   (javax.imageio ImageIO)))

(def ^:const display-scale-factor 20.0)
(def ^:const img-width 32)
(def ^:const img-height 32)
(def ^:const num-channels 3)
(def ^:const desired-generation-population-count 3)

;; https://lospec.com/palette-list/japanese-woodblock
(def ^:const palette--japanesewoodblock
  [[0x2b 0x28 0x21]
   [0x62 0x4c 0x3c]
   [0xd9 0xac 0x8b]
   [0xe3 0xcf 0xb4]
   [0x24 0x3d 0x5c]
   [0x5d 0x72 0x75]
   [0x5c 0x8b 0x93]
   [0xb1 0xa5 0x8d]
   [0xb0 0x3a 0x48]
   [0xd4 0x80 0x4d]
   [0xe0 0xc8 0x72]
   [0x3e 0x69 0x58]])

(def ^:const palette palette--japanesewoodblock)

(defn- vec->image
  ([pixel-vec]
    ;; assume square image
    (let [dim (-> pixel-vec
                  count
                  (/ 3)
                  Math/sqrt
                  int)]
      (def dim dim)
      (vec->image pixel-vec dim dim)))
  ([pixel-vec width height]
   ;; via https://stackoverflow.com/a/125013/1181141
    (let [image  (BufferedImage. width height BufferedImage/TYPE_INT_RGB)
          raster (.getRaster image)]
      (.setPixels ^WritableRaster raster 0 0 width height (int-array pixel-vec))
      image)))

;; width and height are desired dimensions of resultant image -- the return val
;; of this function is a 1D vector of size (* height width num-channels)
(defn- random-vec
  ([]
   (random-vec img-width img-height))
  ([width height]
   (-> (* width height)
       (take (repeatedly #(apply data.gen/one-of palette)))
       flatten)))

(defn- random-img [width height]
  (-> (random-vec width height)
      (vec->image width height)))

(defn- solid-gray-vec [width height val]
  (repeat (* width height num-channels) val))

(defn- black-vec [width height]
  (solid-gray-vec width height 0))

(defn- white-vec [width height]
  (solid-gray-vec width height 255))

(defn- save-img [img-name ^BufferedImage img]
  (ImageIO/write img "png" (File. (format "output/%s.png" img-name))))

;; https://stackoverflow.com/a/4216635
(defn- scale-up [^BufferedImage bi]
  (let [width    (* display-scale-factor (.getWidth bi))
        height   (* display-scale-factor (.getHeight bi))
        after    (BufferedImage. width height BufferedImage/TYPE_INT_RGB)
        xform    (AffineTransform.)
        _        (.scale xform display-scale-factor display-scale-factor)
        scale-op (AffineTransformOp. xform AffineTransformOp/TYPE_NEAREST_NEIGHBOR)]
    (.filter scale-op bi after)))

;; like k-point crossover, but instead of k points, (randomly) crossover after
;; runs of length n
(defn- crossover [n parent0 parent1]
  (let [parent0-partitioned (partition (* n num-channels) parent0)
        parent1-partitioned (partition (* n num-channels) parent1)]
    (flatten
     (for [i (range (count parent0-partitioned))]
       (if (data.gen/boolean)
         (nth parent0-partitioned i)
         (nth parent1-partitioned i))))))

;; get the coords of n patches that evenly cover a width x height 2D array,
;; taking num-channels into account
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
        patch-indices-j (partition (* n num-channels) (range width))]
    (for [i patch-indices-i
          j patch-indices-j]
      [i j])))

(def patches-memo (memoize patches))

(defn- reshape [width flattened-arr]
  (->> flattened-arr
       (partition width)
       (map vec)
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

(defn patch-crossover [n width parent0 parent1]
  ;; loop over the image, patch by patch, taking a given patch from either
  ;; parent randomly
  ;; note: vector dimensions ideally are evenly divided by n
  ;; width param should already be multiplied by num-channels
  (let [parent0-2d (reshape width parent0)
        parent1-2d (reshape width parent1)]
    (flatten
     (loop [[patch & rest-patches] (patches-memo n
                                                 (count (first parent0-2d))
                                                 (count parent0-2d))
            img                    parent0-2d]
       (if patch
         (recur
          rest-patches
          (let [src-img (if (data.gen/boolean) parent0-2d parent1-2d)]
            (reduce
             (fn [res-img pixel]
               (->> (get-in src-img pixel)
                    (assoc-in res-img pixel)))
             img
             patch)))
         img)))))

(comment

  (def vec0 (random-vec img-width img-height))

  (def vec1 (random-vec img-width img-height))

  (def vecblack (black-vec img-width img-height))

  (def vecwhite (white-vec img-width img-height))

  (save-img "vec0" (scale-up (vec->image vec0)))

  (save-img "vec1" (scale-up (vec->image vec1)))

  (save-img "vecblack" (scale-up (vec->image vecblack)))

  (save-img "vecwhite" (scale-up (vec->image vecwhite)))

  (dotimes [n 730]
    (save-img (str "vec-" n) (scale-up (random-img img-width img-height))))

  (let [n 2
        progeny (patch-crossover n
                                 (* img-width num-channels)
                                 vecblack
                                 vecwhite)]
    (save-img (str "patched-" n) (scale-up (vec->image progeny))))

  (let [n 5
        progeny (patch-crossover n
                                 (* img-width num-channels)
                                 vec0
                                 vec1)]
    (save-img (str "progeny-patched-" n) (scale-up (vec->image progeny))))

  )

(defn build-individual [gen-num]
  {::m/id               (java.util.UUID/randomUUID)
   ::m/social-id        nil
   ::m/generation-num   gen-num
   ::m/chromosome       (random-vec)
   ::m/parent0-id       nil
   ::m/parent1-id       nil
   ::m/crossover-method nil
   ::m/crossover-params nil})

;; Genetic Algorithm Tweet Bot
;; solving an opimtization problem: trying to maximize number of impressions

;; seeding
;; selection https://en.wikipedia.org/wiki/Selection_(genetic_algorithm)
;; crossover + mutation

;; links
;; https://en.wikipedia.org/wiki/Genetic_algorithm
;; https://en.wikipedia.org/wiki/Selection_(genetic_algorithm)
;; https://en.wikipedia.org/wiki/Interactive_evolutionary_computation
;; https://en.wikipedia.org/wiki/Evolutionary_art

(defn- normalize-fitness [population]
  (let [fitness-sum (reduce (fn [acc el] (+ (:fitness el) acc))
                            0
                            population)]
    (map #(assoc %
                 :fitness-norm
                 (/
                  (:fitness %)
                  fitness-sum))
         population)))

(defn matchmake [population]
  ;; Fitness proportionate selection
  ;; Given a population, choose two individuals to reproduce.
  ;; https://en.wikipedia.org/wiki/Selection_(genetic_algorithm)
  (let [accumulated-normalized-fitnesses
        (->> population
             ;; The fitness function is evaluated for each individual, providing
             ;; fitness values, which are then normalized.
             (mapv #(assoc % :fitness (social/get-fitness (::m/social-id %))))
             normalize-fitness
             ;; The population is sorted by ascending fitness values.
             (sort-by :fitness-norm)
             ;; Accumulated normalized fitness values are computed: the
             ;; accumulated fitness value of an individual is the sum of its own
             ;; fitness value plus the fitness values of all the previous
             ;; individuals
             (reduce (fn [acc el]
                       (if (empty? acc)
                         [(assoc el :acc-fitness-norm (:fitness-norm el))]
                         (conj acc (assoc el :acc-fitness-norm
                                          (+ (:fitness-norm el)
                                             (:acc-fitness-norm (last acc)))))))
                     []))
        ;; A random number R between 0 and 1 is chosen
        ;; The selected individual is the first one whose accumulated normalized
        ;; value is greater than or equal to R.
        choose-parent #(->> accumulated-normalized-fitnesses
                            (filter (fn [indiv]
                                      (>= (:acc-fitness-norm indiv) (rand))))
                            first)
        parent0       (choose-parent)
        ;; Ensure parent1 is different from parent0. One can't reproduce with
        ;; oneself, unfortunately.
        parent1       (loop [p (choose-parent)]
                        (if (= (:id p) (:id parent0))
                          (recur (choose-parent))
                          p))]
    [parent0 parent1]))

(defn breed [parent0 parent1]
  ;; nsfw
  parent0
  )

(defn -main
  "I don't do a whole lot ... yet."
  [& args]

  ;; TODO mutations -- evolution is stochastic
  ;; TODO meta mutations -- mutate mutation parameters: crossover parameters, crossover methods, etc
  ;; TODO think about generations
  ;; TODO fitness...
  ;; TODO start interfacing with social, database

  ;; if number of individuals in current generation is less than the desired
  ;; generation poulation
  ;;   if generation is 0
  ;;     generate random invididual
  ;;   else
  ;;     glean fitness of inidividuals in prev generation
  ;;     produce new individual by stochastically select two parents from the
  ;;     previous generation, weighted by fitness, and including mutation

  (let [latest-gen-num (m/latest-generation-num)
        latest-gen     (m/read-generation latest-gen-num)
        curr-gen-num   (if (< (count latest-gen) desired-generation-population-count)
                         latest-gen-num
                         (inc latest-gen-num))
        prev-gen       (m/read-generation (dec curr-gen-num))]
    (if (= 0 curr-gen-num)
      (let [new-individual (build-individual curr-gen-num)
            social-id      (social/debut new-individual)]
        (m/create-individual (assoc new-individual ::m/social-id social-id)))
      (let [prev-gen          (m/read-generation (dec curr-gen-num))
            [parent0 parent1] (matchmake prev-gen)
            new-individual    (breed parent0 parent1)])))

  )
