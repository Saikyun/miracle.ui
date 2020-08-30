(ns miracle.playground
  (:require [miracle.clj.save :as s :refer [save ld]]))

(defn inc-all
  [l]
  (save :inc-all)
  (map inc l))

(defn map-all
  [f l]
  (save :map-all)
  (map f l))

(comment
  ;; test data
  (doseq [i (range 10)
          :let [f (rand-nth [inc dec])
                v (vec (repeat 100 (rand-int 30000)))]]
    (map-all f v))
  )
