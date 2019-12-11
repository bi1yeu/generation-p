(ns generation-p.model
  (:require [clojure.java.jdbc :as jdbc]
            [clojure.edn :as edn]
            [clojure.spec.alpha :as s]))

(s/def ::id uuid?)
(s/def ::social-id int?)
(s/def ::generation-num int?)
(s/def ::chromosome (s/coll-of int?))
(s/def ::parent0-id (s/nilable uuid?))
(s/def ::parent1-id (s/nilable uuid?))
(s/def ::patch-crossover "patch-crossover")
(s/def ::crossover "crossover")
(s/def ::crossover-method (s/nilable #{::patch-crossover ::crossover}))
(s/def ::crossover-params (s/nilable map?))
(s/def ::created-at inst?)

(def ^:const ^:private key-map
  {:id               ::id
   :social_id        ::social-id
   :generation_num   ::generation-num
   :chromosome       ::chromosome
   :parent0_id       ::parent0-id
   :parent1_id       ::parent1-id
   :crossover_method ::crossover-method
   :crossover_params ::crossover-params})

(s/def ::individual (s/keys :req [::id
                                  ::social-id
                                  ::generation-num
                                  ::chromosome
                                  ::parent0-id
                                  ::parent1-id
                                  ::crossover-method
                                  ::crossover-params]))

;; via https://github.com/ogrim/clojure-sqlite-example/blob/master/src/clojure_sqlite_example/core.clj
(def ^:const ^:private db
  {:classname   "org.sqlite.JDBC"
   :subprotocol "sqlite"
   :subname     "db/database.db"})

(def ^:const ^:private table-ddl
  (jdbc/create-table-ddl :species
                         [[:id :text]
                          [:social_id :int]
                          [:generation_num :int]
                          [:chromosome :text]
                          [:parent0_id :text]
                          [:parent1_id :text]
                          [:crossover_method :text]
                          [:crossover_params :text]
                          [:created_at :datetime :default :current_timestamp]]))

(defn create-db []
  (try (jdbc/db-do-commands db
                            [table-ddl
                             "CREATE INDEX id_ix ON species ( id );"
                             "CREATE INDEX generation_num_ix ON species ( generation_num );"])
       (catch java.sql.BatchUpdateException e
         (println (.getMessage e)))))

(defn- ns-keywords->field-names [individual]
  (clojure.set/rename-keys individual (clojure.set/map-invert key-map)))

(defn- field-names->ns-keywords [individual]
  (clojure.set/rename-keys individual key-map))

(defn create-individual [individual]
  {:pre  [(s/valid? ::individual individual)]
   :post [(s/valid? ::individual %)]}
  (-> individual
      (update ::chromosome vec)
      ns-keywords->field-names
      (as-> <> (jdbc/insert! db :species <>)))
  individual)

(defn- munge-from-db [individual-record]
  {:post [(s/valid? ::individual %)]}
  ;; this is kinda gross
  (-> individual-record
      (dissoc :created_at)
      field-names->ns-keywords
      (update ::id (fn [v] (java.util.UUID/fromString v)))
      (update ::parent0-id (fn [v] (when v (java.util.UUID/fromString v))))
      (update ::parent1-id (fn [v] (when v (java.util.UUID/fromString v))))
      (update ::chromosome edn/read-string)
      (update ::crossover-method edn/read-string)
      (update ::crossover-params edn/read-string)))

(defn get-individual-by-id [individual-id]
  {:pre  [(s/valid? ::id individual-id)]
   :post [(s/valid? ::individual %)]}
  (-> (jdbc/query db ["SELECT * FROM species WHERE id = ?"
                      individual-id])
      first
      munge-from-db))

(defn get-generation [generation-num]
  {:pre  [(s/valid? int? generation-num)]
   :post [(s/valid? (s/coll-of ::individual :kind vector?) %)]}
  (->>
   (jdbc/query db
               ["SELECT * FROM species WHERE generation_num = ?"
                generation-num])
   (map munge-from-db)
   vec))

(defn latest-generation-num []
  {:post [(s/valid? (s/nilable int?) %)]}
  (let [resultset (jdbc/query db
                              ["SELECT MAX(generation_num) latest_gen FROM species"])]
    (or (:latest_gen (first resultset))
        0)))

(defn latest-generation-count []
  ;; convenience function so we don't have to read the whole generation in just
  ;; to count it
  {:post [(s/valid? (s/nilable int?) %)]}
  (let [resultset
        (jdbc/query db
                    ["SELECT COUNT(*) cnt FROM species WHERE generation_num = (SELECT MAX(generation_num) FROM species)"])]
    (or (:cnt (first resultset))
        0)))
