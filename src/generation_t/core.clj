(ns generation-t.core
  (:gen-class)
  (:import
   (java.io File)
   (java.awt.image AffineTransformOp BufferedImage WritableRaster)
   (java.awt.geom AffineTransform)
   (javax.imageio ImageIO)))

(comment

  ;; master palette
  ;; https://www.pixilart.com/art/flejas-master-palette-8ddc4903c049d7a
  ;; https://lospec.com/palette-list/fleja-master-palette
  (def palette [[0x1f 0x18 0x33]
                [0x2b 0x2e 0x42]
                [0x41 0x48 0x59]
                [0x68 0x71 0x7a]
                [0x90 0xa1 0xa8]
                [0xb6 0xcb 0xcf]
                [0xff 0xff 0xff]
                [0xfc 0xbf 0x8a]
                [0xb5 0x80 0x57]
                [0x8a 0x50 0x3e]
                [0x5c 0x3a 0x41]
                [0xc9 0x30 0x38]
                [0xde 0x6a 0x38]
                [0xff 0xad 0x3b]
                [0xff 0xe5 0x96]
                [0xfc 0xf9 0x60]
                [0xb4 0xd6 0x45]
                [0x51 0xc4 0x3f]
                [0x30 0x9c 0x63]
                [0x23 0x6d 0x7a]
                [0x26 0x4f 0x6e]
                [0x23 0x36 0x63]
                [0x41 0x72 0x91]
                [0x4c 0x93 0xad]
                [0x63 0xc2 0xc9]
                [0x94 0xd2 0xd4]
                [0xb8 0xfd 0xff]
                [0x3c 0x29 0x40]
                [0x46 0x27 0x5c]
                [0x82 0x64 0x81]
                [0xf7 0xa4 0x8b]
                [0xc2 0x71 0x82]
                [0x85 0x2d 0x66]])

  ;; via https://stackoverflow.com/a/125013/1181141
  (defn get-image-from-array [pixels width height]
    (let [image  (BufferedImage. width height BufferedImage/TYPE_INT_RGB)
          raster (.getRaster image)]
      (.setPixels ^WritableRaster raster 0 0 width height pixels)
      image))

  (get-image-from-array (int-array (flatten a)) 64 64)

  (defn random-img [width height]
    (-> (* width height)
        (take (repeatedly (partial rand-nth palette)))
        flatten
        int-array
        (get-image-from-array width height)))

  (def img (random-img 64 64))

  (defn- save-img [img-name ^BufferedImage img]
    (ImageIO/write img "png" (File. (str "output/"
                                         img-name
                                         ".png"))))

  (def ^:const display-scale-factor 10.0)

  ;; https://stackoverflow.com/a/4216635
  (defn- scale-up [^BufferedImage bi]
    (let [width    (* display-scale-factor (.getWidth bi))
          height   (* display-scale-factor (.getHeight bi))
          after    (BufferedImage. width height BufferedImage/TYPE_INT_RGB)
          xform    (AffineTransform.)
          _        (.scale xform display-scale-factor display-scale-factor)
          scale-op (AffineTransformOp. xform AffineTransformOp/TYPE_NEAREST_NEIGHBOR)]
      (.filter scale-op bi after)))

  (save-img "orig" img)
  (save-img "test" (scale-up img))

  )

(defn- rand-tweet []
  (org.apache.commons.lang3.RandomStringUtils/randomAscii 280))

;; TODO k-point crossover?
;; TODO more than two parents?
(defn- crossover [parent0 parent1]
  (apply str
         (for [i (range 280)]
           (if (= 0 (rand-int 2))
             (nth (vec parent0) i)
             (nth (vec parent1) i)))))

(comment

  (reduce (fn [acc el]
            (println acc)
            (println "----------")
            (println el)
            (println "-------------------------")
            ;; (Thread/sleep 1000)
            (crossover acc el))
          (rand-tweet)
          (repeatedly 60 rand-tweet))

  )

;; Genetic Algorithm Tweet Bot
;;
;; The population is initially seeded with N randomly generated tweets
;; A chromosome is represented as 280 ASCII characters
;;   - should it use all utf-8 characters? only certain subset?
;; Uniform crossover https://en.wikipedia.org/wiki/Crossover_(genetic_algorithm)#Uniform_crossover
;;   - would a k-point crossover produce more interesting output?
;; Fitness function is based on likes and retweets
;; TODO mutation?
;;   - capitalize
;;   - space/punctuate
;;   - simply change char
;;   - mutate crossover rule?
;;   - mutate hyper parameters?
;; Tweet twice per day
;; solving an opimtization problem: trying to maximize number of impressions
;;
;; Each generation, replace 50% of the population

;; Seed `n` individuals without reproducing
;; e.g. n = 4
;; gen 0:
;;   t0 abc 1
;;   t1 def 3
;;   t2 ghi 2
;;   t3 jkl 10

;; then selection https://en.wikipedia.org/wiki/Selection_(genetic_algorithm)
;; jkl 0.625  1       r == 0.5 <--
;; def 0.1875 0.375   r == 0.3 <--
;; ghi 0.125  0.1875
;; abc 0.0625 0.0625

;; then crossover + mutation
;; gen 1:
;;   t4 jkl + def = jei 1
;;            mut-> jeI
;;   (selection)
;;   t5 jkl + def = del 2
;;   (selection)
;;   t6 del + def = def ;; speciation heuristic?

;; fitness heurisitc
;; - if impressions alone form the basis of the fitness heuristic, then older
;;  tweets would be disproportionately fit due simply to having had more time to
;;  receive impressions.
;;  i think it should be something like fitness = a * impressions - b * age
;;  super old and/or unfit individuals would thus effictively be culled from the
;;  gene pool due to v low probability of being selected

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
