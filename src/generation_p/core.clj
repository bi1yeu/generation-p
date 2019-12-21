(ns generation-p.core
  (:gen-class)
  (:require [clojure.data.generators :as data.gen]
            [clojure.tools.logging :as log]
            [generation-p.model :as m]
            [generation-p.social :as social]
            [generation-p.biology :as bio]))

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

(defn -main [& _]
  (binding [data.gen/*rnd* (java.util.Random.)]
    (m/create-db)
    (let [latest-gen-num (m/latest-generation-num)
          curr-gen-num   (if (< (m/latest-generation-count)
                                bio/desired-generation-population-count)
                           latest-gen-num
                           (inc latest-gen-num))
          new-individual (build-individual curr-gen-num)
          _              (log/infof "Generated individual %s" (::m/id new-individual))
          social-id      (social/debut new-individual)]
      (log/infof "Posted individual %s" (::m/id new-individual))
      (m/create-individual (assoc new-individual ::m/social-id social-id))
      (log/infof "Saved individual %s" (::m/id new-individual))
      (social/cleanup-connection)
      (log/info "Exiting"))))

(comment

  (-main)


  )
