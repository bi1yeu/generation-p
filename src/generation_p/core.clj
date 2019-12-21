(ns generation-p.core
  (:gen-class)
  (:require [generation-p.model :as m]
            [generation-p.social :as social]
            [generation-p.biology :as bio]
            [clojure.data.generators :as data.gen]))

(defmulti build-individual identity)

(defmethod build-individual 0 [_]
  (bio/spawn-random-individual))

(defmethod build-individual :default [curr-gen-num]
  (let [prev-gen          (m/get-generation (dec curr-gen-num))
        ;; TODO could probably save some memory if we defer fetching
        ;; chromosome vecs from the db until such point as we actually, you
        ;; know, need them for breeding. Fitness is determined w/o them.
        [parent0 parent1] (bio/matchmake prev-gen)]
    (bio/breed parent0 parent1)))

(defn -main [& args]
  (binding [data.gen/*rnd* (java.util.Random.)]
    (m/create-db)
    (let [latest-gen-num (m/latest-generation-num)
          curr-gen-num   (if (< (m/latest-generation-count)
                                bio/desired-generation-population-count)
                           latest-gen-num
                           (inc latest-gen-num))
          new-individual (build-individual curr-gen-num)
          social-id      (social/debut new-individual)]
      (m/create-individual (assoc new-individual ::m/social-id social-id))
      (println (format "Created individual %s" (::m/id new-individual))))))

(comment

  (-main)


  )
