(ns miracle.visualize
  (:require [parcera.core :as parcera]
            
            [clojure.data :as data]
            [clojure.pprint :refer [pp pprint]]
            [clojure.repl :refer [source]]
            [potemkin.walk :refer [postwalk]]
            [clojure.walk :refer [macroexpand-all]]))

(defn binary-search
  [coll v]
  (loop [min 0
         max (count coll)]
    (let [pos (/ (- max min) 2)
          lookup (get coll pos)]
      (cond (< v lookup) (recur min pos)
            (> v lookup) (recur pos max)
            (= v lookup) lookup))))


(comment
  ;; visualize-defn
  (defn binary-search
    [coll v]                                 ; coll [1 2 3], v 1
    (loop [min 0                             ; min    0 | 0
           max (count coll)]                 ; max    3 | 1
      (let [pos (quot (- max min) 2)         ; pos    1 | 0
            lookup (get coll pos)]           ; lookup 2 | 1
        (cond (< v lookup) (recur min pos)
              (> v lookup) (recur pos max)
              (= v lookup) lookup)))))        ; ::ret 1

(defn id!
  [func]
  0)

(defn start-record!
  "Called at the beginning of a recording session.
  Takes an identifier (in general, the function name), then generates an id for the current (function) call.
  Returns an ID that is used to store additional bindings, and signal the end of the recording.
  "
  [func]
  (id! func))

(defmacro rec!
  [call-id form]
  `(let [res# ~form]
     (println "saving" res# "in id" ~call-id)
     (println "env: " ~(meta &form))
     res#))

(defn end-record!
  "Stops a record and stores the final result."
  [call-id res]
  (println "done with " call-id " returns " res)
  res
  )

(defn error-record!
  "When an error is encountered, store it and re-throw it."
  [call-id err]
  (println "error in " call-id "!")
  (throw err))

(defn populate-bindings
  "Traverse the form and look for bindings, e.g. let, loop and for.
  Replace the values with calls to `(rec! call-id ...)`.
  
  E.g.
  (let [a 10]) => (let [a (rec! call-id 10)])
  "
  [call-id form]
  `(rec! ~call-id ~form)
  )

(defmacro with-record
  [code-id & body]
  (let [id-sym (gensym "id")
        call-id (start-record! code-id)]
    `(let [~id-sym ~call-id]
       (try (let [res# (do ~@(map #(populate-bindings call-id %) body))]
              (end-record! ~id-sym res#))
            (catch Throwable t#
              (error-record! ~id-sym t#))))))

(defmacro popper
  [& body]
  (doseq [f body]
    (println f (meta f))
    (when (coll? f)
      (doseq [f f]
        (println f (meta f))
        (when (coll? f)
          (doseq [f f]
            (println f (meta f))))
        (println)))
    (println))
  `(do ~@body)
  )

(defn print-code [o]
  (binding [clojure.pprint/*print-right-margin* 100
            clojure.pprint/*print-miser-width* 60]
    (clojure.pprint/with-pprint-dispatch clojure.pprint/code-dispatch
      (clojure.pprint/pprint o))))

(comment
  
  (map #(populate-bindings 123 %) '((+ 1 1)))
  
  (print-code (macroexpand-all '(defn binary-search
                                  [coll v]
                                  (with-record ::binary-search
                                    (->> (loop [min 0
                                                max (count coll)]
                                           (let [pos    (quot (- max min) 2)
                                                 lookup (get coll pos)]
                                             (cond (< v lookup) (recur min pos)
                                                   (> v lookup) (recur pos max)
                                                   (= v lookup) lookup)))))))))


;; visualize-defn
(defn binary-search
  [coll v]
  (with-record ::binary-search
    (->> (loop [min 0
                max (count coll)]
           (let [pos    (quot (- max min) 2)
                 lookup (get coll pos)]
             (cond (< v lookup) (recur min pos)
                   (> v lookup) (recur pos max)
                   (= v lookup) lookup))))))

(eval (read-string
       (parcera/code (parcera/ast
                      "(defn binary-search
                    [coll v]
                    (with-record ::binary-search
                      (->> (loop [min 0
                                  max (count coll)]
                             (let [pos    (quot (- max min) 2)
                                   lookup (get coll pos)]
                               (cond (< v lookup) (recur min pos)
                                     (> v lookup) (recur pos max)
                                     (= v lookup) lookup))))))"
                      ))))


(defn remove-whitespace
  [l]
  (remove #(and (seq? %) (= (first %) :whitespace)) l))

(defmulti handle-list first)

(defmethod handle-list '(:symbol "let")
  [[tag & body :as node]]
  #_(println "yoo" (map (comp meta second) (remove-whitespace body)))
  
  #_(prn "huh " body)
  
  (let [[before [bindings & body]] (split-with #(not (and (seq? %) (= :vector (first %)))) body)
        [sym & bs] bindings
        
        new-bindings (concat (list sym)
                             (loop [current-sym nil
                                    [[tag v :as b] & bs] bs
                                    res '()]
                               (if-not b
                                 res
                                 (cond (and (nil? current-sym)
                                            (= tag :symbol)) 
                                       (recur v bs (concat res [b]))
                                       
                                       (and (some? current-sym)
                                            (not= tag :whitespace)) 
                                       (recur nil bs (concat res
                                                             [(list :list
                                                                    (list :symbol "rec!")
                                                                    (list :whitespace " ")
                                                                    (list
                                                                     :list (list :symbol "quote")
                                                                     (list :whitespace " ")
                                                                     (parcera/ast (str (meta v))))
                                                                    (list :whitespace " ")
                                                                    (list :list
                                                                          (list :symbol "quote")
                                                                          (list :whitespace " ")
                                                                          (list :symbol current-sym))
                                                                    (list :whitespace " ")
                                                                    b)]))
                                       
                                       :else (recur current-sym bs (concat res [b]))))))]
    
    (pprint new-bindings)
    
    #_    (prn "hehu" (partition 3 bindings))
    
    #_    (println "same" (= (concat (list tag)
                                     before
                                     bindings
                                     body)
                             node))
    
    (assert (= node (concat (list tag)
                            before
                            (list bindings)
                            body)))
    
    (concat (list tag)
            before
            (list new-bindings)
            body)
    
    
    #_(pprint (concat (list tag)
                      before
                      bindings
                      body))
    
    #_(parcera/code (concat (list tag)
                            before
                            bindings
                            body)))
  
  #_  node)

(defmethod handle-list :default
  [node]
  node)

(defmulti handle-node first)

(defmethod handle-node :list
  [[tag & data :as node]]
  (apply list tag (postwalk #(if (seq? %) (apply list %) %) (handle-list data))))

(defmethod handle-node :vector
  [[tag data :as node]]
  node)

(defmethod handle-node :default
  [node]
  node)

#_(postwalk (fn [node]
              (if (seq? node)
                (handle-node node)
                node)) ast)

(def ast (parcera/ast
          "(defn binary-search
                    [coll v]
                    (with-record ::binary-search
                      (->> (loop [min   0
                                  max (count coll)]
                             (let [pos (quot (- max min) 2)
                                   lookup (get coll pos)]
                               (cond (< v lookup) (recur min pos)
                                     (> v lookup) (recur pos max)
                                     (= v lookup) lookup))))))"))

(defn parcera->enlive
  [form]
  (if (seq? form)
    {:tag     (first form)
     :attrs   (meta form)
     :content (map parcera->enlive (rest form))}
    form))

(defn enlive->parcera
  [form]
  (if (map? form)
    (let [{:keys [tag attrs content]} form]
      (with-meta (apply list tag (map enlive->parcera content)) attrs))
    form))

(= ast (enlive->parcera (parcera->enlive ast)))

#_(pprint (parcera->enlive (parcera/ast "(let [a 10] (+ a a))")))

#_(print-code (read-string (parcera/code (postwalk (fn [node]
                                                     (if (seq? node)
                                                       (handle-node node)
                                                       node)) ast))))

#_(binary-search [1 2 3] 2)


(comment
  ;; (defn binary-search
  ;;   [coll v]
  ;;   (with-record ::binary-search
  ;;     (->> (loop [min (rec! id 'min 0)
  ;;                 max (rec! id 'max (count coll))]
  ;;            (let [pos    (rec! id 'pos (quot (- max min) 2))
  ;;                  lookup (rec! id 'lookup (get coll pos))]
  ;;              (cond (< v lookup) (recur min pos)
  ;;                    (> v lookup) (recur pos max)
  ;;                    (= v lookup) lookup))))))
  )
