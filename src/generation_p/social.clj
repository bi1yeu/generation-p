(ns generation-p.social
  (:require [clojure.java.io :as io]
            [twitter.core :as tw.core]
            [twitter.oauth :as tw.oauth]
            [twitter.api.restful :as tw.api]
            [twitter.request :as tw.request]
            [generation-p.model :as m]
            [generation-p.image :as image]))

(def ^:const ^:private retweet-to-favorite-weighting 30)

(defn- is-prod? [] (= "prod" (System/getenv "ENV")))

;; via https://github.com/adamwynne/twitter-api
(def ^:private creds
  (tw.oauth/make-oauth-creds (System/getenv "CONSUMER_KEY")
                             (System/getenv "CONSUMER_SECRET")
                             (System/getenv "ACCESS_TOKEN")
                             (System/getenv "SECRET_KEY")))

(defn- make-status-message [individual]
  (format "generation %s\n%s\nparents\n  - %s\n  - %s"
          (::m/generation-num individual)
          (::m/id individual)
          (::m/parent0-id individual)
          (::m/parent1-id individual)))

;; TODO API error handling
(defn debut [individual]
  (if (not (is-prod?))
    (do (image/save-individual-as-image individual)
        (rand-int 100000))
    (let [filename   (image/save-individual-as-image individual)
          status-msg (make-status-message individual)
          ;; note: the media-upload-chunked function in the docs isn't published
          status     (tw.api/statuses-update-with-media
                      :oauth-creds
                      creds
                      :body [(tw.request/file-body-part filename)
                             (tw.request/status-body-part status-msg)])]
      (io/delete-file filename)
      (-> status :body :id))))

(defn get-fitness [individual]
  (if (not (is-prod?))
    (rand-int 20)
    (let [status (-> (tw.api/statuses-show-id :oauth-creds
                                              creds
                                              :params {:id (::m/social-id individual)})
                     :body)]
      (+ (* retweet-to-favorite-weighting
            (:retweet_count status))
         (:favorite_count status)))))

;; via https://github.com/adamwynne/twitter-api#notes-on-making-api-calls
(defn cleanup-connection []
  (when (is-prod?)
    (http.async.client/close (tw.core/default-client))))

(comment

  ;; DANGER!!!
  (defn- destroy-status [individual]
    (tw.api/statuses-destroy-id :oauth-creds
                                creds
                                :params {:id (::m/social-id individual)}))

  (dotimes [n 0]
    (run! destroy-status (m/get-generation n)))

  ;; DANGER!!!


  )
