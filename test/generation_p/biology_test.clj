(ns generation-p.biology-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.data.generators :as data.gen]
            [generation-p.biology :as bio]))

(def ^:const rand-seed 666)

(deftest patch-crossover-test
  (binding [data.gen/*rnd* (java.util.Random. rand-seed)]
    (testing "single pixel patch crossover"
      (let [params  {:n 1}
            parent0 [[0 0 0] [0 0 0]
                     [0 0 0] [0 0 0]]
            parent1 [[1 1 1] [1 1 1]
                     [1 1 1] [1 1 1]]]
        (is (= [[1 1 1] [1 1 1]
                [1 1 1] [0 0 0]]
               (#'bio/patch-crossover params parent0 parent1)))))
    (testing "2x2 pixel patch crossover"
      (let [params  {:n 2}
            parent0 [[0 0 0] [0 0 0] [0 0 0] [0 0 0]
                     [0 0 0] [0 0 0] [0 0 0] [0 0 0]
                     [0 0 0] [0 0 0] [0 0 0] [0 0 0]
                     [0 0 0] [0 0 0] [0 0 0] [0 0 0]]
            parent1 [[1 1 1] [1 1 1] [1 1 1] [1 1 1]
                     [1 1 1] [1 1 1] [1 1 1] [1 1 1]
                     [1 1 1] [1 1 1] [1 1 1] [1 1 1]
                     [1 1 1] [1 1 1] [1 1 1] [1 1 1]]]
        (is (= [[0 0 0] [0 0 0] [1 1 1] [1 1 1]
                [0 0 0] [0 0 0] [1 1 1] [1 1 1]
                [1 1 1] [1 1 1] [0 0 0] [0 0 0]
                [1 1 1] [1 1 1] [0 0 0] [0 0 0]]
               (#'bio/patch-crossover params parent0 parent1)))))))

(deftest crossover-test
  (binding [data.gen/*rnd* (java.util.Random. rand-seed)]
    (let [parent0 [[0 0 0] [0 0 0]
                   [0 0 0] [0 0 0]]
          parent1 [[1 1 1] [1 1 1]
                   [1 1 1]  [1 1 1]]]
      (testing "single pixel crossover"
        (is (= [[1 1 1] [1 1 1]
                [1 1 1] [0 0 0]]
               (#'bio/crossover {:n 1} parent0 parent1))))
      (testing "multi pixel crossover"
        (is (= [[0 0 0] [0 0 0]
                [1 1 1] [1 1 1]]
               (#'bio/crossover {:n 2} parent0 parent1)))))))
