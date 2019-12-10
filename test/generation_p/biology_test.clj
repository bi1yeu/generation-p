(ns generation-p.biology-test
  (:require [clojure.test :refer :all]
            [clojure.data.generators :as data.gen]
            [generation-p.biology :refer :all]
            [generation-p.image :as image]))

(def ^:const rand-seed 666)

;; TODO failing because img-width

(deftest patch-crossover-test
  (with-redefs [data.gen/*rnd*  (java.util.Random. rand-seed)]
    (let [params  {:n 1}
          parent0 [0 0 0  0 0 0
                   0 0 0  0 0 0]
          parent1 [255 255 255  255 255 255
                   255 255 255  255 255 255]]
      (is (= [255 255 255  255 255 255
              255 255 255  0   0   0]
             (patch-crossover params parent0 parent1))))
    (let [params  {:n 2}
          parent0 [0 0 0  0 0 0  0 0 0  0 0 0
                   0 0 0  0 0 0  0 0 0  0 0 0
                   0 0 0  0 0 0  0 0 0  0 0 0
                   0 0 0  0 0 0  0 0 0  0 0 0]
          parent1 [1 1 1  1 1 1  1 1 1  1 1 1
                   1 1 1  1 1 1  1 1 1  1 1 1
                   1 1 1  1 1 1  1 1 1  1 1 1
                   1 1 1  1 1 1  1 1 1  1 1 1]]
      (is (= [0 0 0  0 0 0  1 1 1  1 1 1
              0 0 0  0 0 0  1 1 1  1 1 1
              1 1 1  1 1 1  0 0 0  0 0 0
              1 1 1  1 1 1  0 0 0  0 0 0]
             (patch-crossover params parent0 parent1))))))
