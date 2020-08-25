(ns miracle.playground
  (:require [miracle.ui :as ui]
            [miracle.clj.save :as s :refer [save ld]]))

(defn inc-all
  [l]
  (save :inc-all)
  (map inc l))

(defn map-all
  [f l]
  (save :map-all)
  (map f l))

(comment
  (ui/start-ui-server!)
  
  (inc-all [1 2 3])
  
  (map-all inc [1 2 3])
  (map-all inc [4 2 19 1209 32190312 12390])
  
  (s/print-saves :inc-all)
  )
