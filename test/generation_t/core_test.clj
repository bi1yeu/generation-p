(ns generation-p.core-test
  (:require [clojure.test :refer :all]
            [generation-p.core :refer :all]
            [clojure.data.generators :as data.generators]))

(def ^:const rand-seed 666)

(deftest patch-crossover-test
  (with-redefs [data.generators/*rnd* (java.util.Random. rand-seed)]
    (let [n 1
          width 6
          parent0 [0 0 0 0 0 0 0 0 0 0 0 0]
          parent1 [255 255 255 255 255 255 255 255 255 255 255 255]]
      (is (= (patch-crossover n width parent0 parent1)
             [0 0 0 255 255 255 0 0 0 255 255 255])))))
