(ns generation-p.core
  (:gen-class)
  (:require [generation-p.model :as m]
            [generation-p.social :as social]
            [generation-p.biology :as bio]))

;; Genetic Algorithm Pixel Art Bot
;; Solving an opimtization problem: trying to maximize number of impressions

;; resources
;; https://en.wikipedia.org/wiki/Genetic_algorithm
;; https://en.wikipedia.org/wiki/Selection_(genetic_algorithm)
;; https://en.wikipedia.org/wiki/Interactive_evolutionary_computation
;; https://en.wikipedia.org/wiki/Evolutionary_art

(defn -main
  "I don't do a whole lot ... yet."
  [& args]

  ;; TODO mutations -- evolution is stochastic
  ;; TODO convergence?
  ;; TODO program perf? sprinkle some pmaps around idk
  ;; TODO fitness...
  ;; TODO start interfacing with social

  (m/create-db)

  (dotimes [n (* bio/desired-generation-population-count 100)]
    (let [latest-gen-num   (m/latest-generation-num)
          curr-gen-num     (if (< (m/latest-generation-count)
                                  bio/desired-generation-population-count)
                             latest-gen-num
                             (inc latest-gen-num))]
      (if (= 0 curr-gen-num)
        (let [new-individual (bio/spawn-random-individual)
              social-id      (social/debut new-individual)]
          (m/create-individual (assoc new-individual ::m/social-id social-id)))
        (let [prev-gen          (m/get-generation (dec curr-gen-num))
              ;; TODO could probably save some work if we defer fetching
              ;; chromosome vecs from the db until such point as we actually,
              ;; you know, need them for breeding.
              [parent0 parent1] (bio/matchmake prev-gen)
              new-individual    (bio/breed parent0 parent1)
              social-id         (social/debut new-individual)]
          (generation-p.image/save-individual-as-image new-individual)
          (m/create-individual (assoc new-individual ::m/social-id social-id))
          ))))

  )

(comment

  (def social-id (social/debut (bio/spawn-random-individual)))

  (social/get-fitness {::m/social-id social-id})

  (time (bio/spawn-random-individual))
  ;; 0.15 msecs

  (def rando (bio/spawn-random-individual))

  (dotimes [n 60]
    (let [mutated (assoc rando ::m/chromosome (bio/mutate (::m/chromosome rando)))]
      (generation-p.image/save-individual-as-image mutated (format "%d-%s.png" n (::m/id rando)))))

  (time
   (dotimes [n 1000]
     (m/latest-generation-num)
     (let [rando (bio/spawn-random-individual)]
       (m/create-individual (assoc rando ::m/social-id 123)))))
  ;; 62k seconds

  (time (m/create-individual (assoc rando ::m/social-id 123)))
  ;; 30 msecs

  (let [gen (m/get-generation 30)]
    (count gen)
    )

  (m/latest-generation-count)
  (m/create-db)

  )
