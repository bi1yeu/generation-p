(ns generation-p.biology-test
  (:require [clojure.test :refer :all]
            [generation-p.biology :refer :all]
            [clojure.data.generators :as data.generators]))

(def ^:const rand-seed 666)

(deftest patch-crossover-test
  (with-redefs [data.generators/*rnd* (java.util.Random. rand-seed)]
    (let [n 1
          width 6
          parent0 [0 0 0  0 0 0
                   0 0 0  0 0 0]
          parent1 [255 255 255  255 255 255
                   255 255 255  255 255 255]]
      (is (= [0 0 0  255 255 255
              0 0 0  255 255 255]
             (patch-crossover n width parent0 parent1))))
    (let [n 2
          width 12
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
             (patch-crossover n width parent0 parent1))))))
