(ns generation-p.model
  (:require [clojure.java.io :as io]
            [clojure.spec.alpha :as s])
  (:import [java.util Date]))

(s/def ::id uuid?)
(s/def ::social-id int?)
(s/def ::generation int?)
(s/def ::chromosome (s/coll-of int?))
(s/def ::parent0-id (s/nilable uuid?))
(s/def ::parent1-id (s/nilable uuid?))
(s/def ::crossover-method #{"patch-crossover" "crossover"})
(s/def ::crossover-params map?)
(s/def ::created-at inst?)

(s/def ::individual (s/keys :req [::id
                                  ::social-id
                                  ::generation
                                  ::chromosome
                                  ::parent0-id
                                  ::parent1-id
                                  ::crossover-method
                                  ::crossover-params]
                            :opt [::created-at]))

;; For now, the database is just a line-delimited, append-only flat EDN file
(def ^:private ^:const db-filename "db.edn")

(defn- db-file-exists? []
  (.exists (io/as-file db-filename)))

(defn create-individual [individual]
  {:pre  [(s/valid? ::individual individual)]
   :post [(s/valid? ::individual %)]}
  (let [individual-to-create (assoc individual ::created-at (Date.))]
    (with-open [wrtr (io/writer db-filename :append true)]
      (.write wrtr (pr-str individual-to-create))
      (.newLine wrtr))
    individual-to-create))

(defn read-generation [generation-num]
  {:pre  [(s/valid? int? generation-num)]
   :post [(s/valid? (s/coll-of ::individual :kind vector?) %)]}
  (if (db-file-exists?)
    (with-open [rdr (io/reader db-filename)]
      (->> rdr
           line-seq
           (map clojure.edn/read-string)
           (filter #(= generation-num (::generation %)))
           vec))
    []))

(defn latest-generation []
  {:post [(s/valid? (s/nilable int?) %)]}
  (when (db-file-exists?)
    (with-open [rdr (io/reader db-filename)]
      (-> rdr
          line-seq
          last
          clojure.edn/read-string
          ::generation))))
