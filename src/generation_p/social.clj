(ns generation-p.social
  (:require [generation-p.model :as m]))

(defn debut [individual]
  ;; TODO
  ;; post to social and return social-id
  (rand-int 100000))

(defn get-fitness [individual]
  ;; TODO
  ;; get fitness proxy from previously-debuted social post
  (/ (apply + (::m/chromosome individual))
     (count (::m/chromosome individual))))
