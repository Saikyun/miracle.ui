(ns miracle.ui
  (:require [miracle.clj.save :as s :refer [save ld]]
            [clojure.string :as str]
            [miracle.playground]
            
            [clojure.pprint :refer [pprint pp]])
  (:import [java.net ServerSocket Socket SocketException]
           [java.io InputStreamReader OutputStreamWriter]
           [clojure.lang LineNumberingPushbackReader]))

(let [stdout *out*]
  (def std-println #(binding [*out* stdout] (apply println %&))))

(defn on-thread [f]
  (doto (new Thread f)
    (.start)))

(defn create-server 
  "creates and returns a server socket on port, will pass the client
  socket to accept-socket on connection" 
  [accept-socket port]
  (let [ss (new ServerSocket port)]
    (on-thread #(when-not (. ss (isClosed))
                  (try (accept-socket (. ss (accept)))
                       (catch SocketException e
                         (println "ERROR" e)))
                  (recur)))
    ss))

(defn inspect-map
  [map-to-print & {:keys [desired-level safe-count right-margin]
                   :or {desired-level 4 safe-count 10 right-margin 40}}]
  (binding [*print-level* desired-level *print-length* safe-count
            clojure.pprint/*print-right-margin* right-margin]
    (clojure.pprint/pprint map-to-print)))

(declare ^:dynamic *temp-bindings*)

(defn validate-state
  [{:keys [nof-save-lines] :as state}]
  (update state :pos (fn [pos] (max 0 pos))))

(defn fulfills-filters?
  [filters bindings]
  (if (seq filters)
    (apply = true
           (for [[k pred] filters
                 :let [;;str-filter #(str/includes? (str %) pred)
                       k (symbol (last (str/split k #"-")))]]
             (do (save :inner-filter)
                 (if (cond (str/starts-with? pred "#'") 
                           (try ((eval (read-string pred)) (get bindings k))
                                (catch Exception _
                                  true
                                  ;;(str-filter (get bindings k))
                                  ))
                           
                           (str/starts-with? pred "#")
                           (try ((eval (read-string pred)) (get bindings k))
                                (catch Exception _
                                  true
                                  ;; (str-filter (get bindings k))
                                  ))
                           
                           :else
                           (let [pred (if (str/starts-with? pred "(")
                                        pred
                                        (str "(" pred ")"))]
                             (try (binding [*temp-bindings* bindings]
                                    (eval `(let [~k (get *temp-bindings* '~k)]
                                             ~(read-string pred))))
                                  (catch Exception e
                                    ;;(std-println e)
                                    true
                                    ;;(str-filter (get bindings k))
                                    )))
                           
                           ;;:else true
                           ;; (str-filter (get bindings k))
                           )
                   true
                   nil))))
    bindings))

(defn row
  [{:keys [bindings selected columns width] :as props}]
  (into [:div {:display  :grid
               :selected selected}]
        (for [k columns
              :let [v (get bindings k)
                    s (with-out-str (inspect-map v))
                    s (subs s 0 (dec (count s)))]] ;; remove trailing \n
          [:div (if (and width (< width (count s)))
                  (subs s 0 width)
                  s)])))

(defn hiccup
  [{:keys [filters pos height save-key width] :as state}]
  (let [saves         (get @miracle.clj.save/saves save-key)
        ks            (into #{} (mapcat (comp keys second) saves))
        before        2
        after         2
        divider       3
        pos           (max pos 0)
        skip          (quot pos 3)
        column-width
        , (if (empty? ks)
            width
            (quot (- width
                     (* divider (dec (count ks)))
                     (+ before after))
                  (count ks)))
        form-lines
        , [:div [:p save-key]
           (into [:div {:display :grid}]
                 (for [k ks]
                   [:div k]))
           (into [:div {:display :grid}]
                 (for [k ks]
                   [:input {:id (str "filter-" k)
                            :value (or (get filters (str "filter-" k)) "")}]))]          
        filtered-bindings
        , (filter (fn [[_ bindings]] (fulfills-filters? filters bindings)) saves)
        filtered-bindings2
        , (drop (* 3 skip) filtered-bindings) 
        filtered-bindings3
        , (if (empty? filtered-bindings2)
            (if-let [e (last filtered-bindings)] [e] [])
            (vec (take 10 filtered-bindings2)))
        new-pos
        , (if (seq filtered-bindings2)
            pos
            (max 0 (dec (count filtered-bindings))))
        [id selected-bindings]
        , (get filtered-bindings3 (if (= 0 skip)
                                    new-pos
                                    (rem new-pos (* skip 3))))
        save-lines
        , (vec (for [[_ bindings] filtered-bindings3
                     :let [selected (identical? bindings selected-bindings)]]
                 (row {:bindings bindings
                       :columns  ks
                       :width    (if selected nil column-width)
                       :selected selected})))]
    (-> {:filter-line 4
         :save-key save-key
         :width width
         :filters filters
         :pos new-pos
         :selected-save id
         :ranges (-> (map-indexed (fn [i k] {:arg k
                                             :start (+ before (* i column-width))
                                             :end (+ before (* (inc i) column-width))})
                                  ks)
                     vec)
         :nof-save-lines (count save-lines)
         :lines (vec (concat form-lines save-lines))}
        validate-state)))

(comment
  (identical? (first filtered-bindings) (get filtered-bindings (if (= 0 le-hurp)
                                                                 pos
                                                                 (mod pos le-hurp))))
  )

(defonce state (atom {:filters {}
                      :pos 0
                      :save-key :map-all
                      :width 80}))

(hiccup @state)

(comment
  (reset! miracle.clj.save/saves {})
  
  )

(comment
  (eval-in-selected-context (conj l 1))
  
  (conj l 1)
  
  (-> @miracle.clj.save/saves
      (get :map-all) 
      last
      first)
  
  )

(defmacro eval-in-selected-context
  [& body]
  `(miracle.clj.save/eval-in-context (do ~@body) ~:map-all ~(:selected-save @state)))

(comment
  (keys (:selected-save @state))
  
  (macroexpand '(miracle.clj.save/eval-in-context (conj l 10) :map-all 3))
  
  (macroexpand '(miracle.clj.save/eval-in-context form :map-all (:selected-save @state)))
  
  (macroexpand '(eval-in-selected-context (conj l 10)))
  
  (eval-in-selected-context (conj l 10) (conj l 20))
  )

(defn handle-input
  [in println]
  (save :in)
  (try
    (cond
      (= in 'a) (do (save :print)
                    (binding [*out* *out*]
                      (prn (:lines (hiccup @state))))
                    (println "got command" in))
      
      (vector? in) (let [{:keys [value] :as command} (apply hash-map in)
                         new-state (swap! state assoc-in [:filters (:assoc command)] value)]
                     #_(println "printing" new-state)
                     (prn (:lines (hiccup new-state)))
                     (println "done")
                     #_(println (:lines (hiccup :map-all 80 new-state))))
      
      (coll? in) (let [f (eval in)
                       new-state (swap! state #(hiccup (f %)))]
                   ;;(std-println "got msg" in (pr-str (:lines new-state)))
                   (prn (:lines new-state)))
      
      :else (do (save :unreq)
                #_(prn "unreq" e)
                (println "unreeee" in)))
    (catch Exception e
      (save :err)
      (println "Got error: " e)
      (prn {:error (with-out-str (pprint (Throwable->map e)))}))))

(defn print-all
  [ins outs]
  (let [stdout *out*]
    (binding [*out* (new OutputStreamWriter outs)]
      (let [r (new LineNumberingPushbackReader (new InputStreamReader ins))
            eof (Object.)]
        (loop [e (read r false eof)]
          (when-not (= e eof)
            (handle-input e #(binding [*out* stdout] (apply println %&)))
            (flush)
            (recur (read r false eof))))))))

(comment
  )

(def octal-char
  (str "o"
       "("
       , "([0-3][0-7][0-7])"
       , "|"
       , "([0-7][0-7])"
       , "|"
       , "([0-7])"
       ")"))

(defonce servers (atom {}))

(defn start-ui-server!
  ([] (start-ui-server! {}))
  ([{:keys [port] :or {port 4433}}]
   (when-let [s (create-server
                 (fn [s]
                   (on-thread #(print-all
                                (. s (getInputStream))
                                (. s (getOutputStream)))))
                 port)]
     (swap! servers assoc port s))))

(defn -main
  []
  (doseq [i (range 3)
          :let [f (rand-nth [inc dec])
                v (vec (repeat 3 (rand-int 10)))]]
    (miracle.playground/map-all f v))
  
  (start-ui-server!)
  
  (println "UI server running on" 4433)

  (def client (new Socket "localhost" 4433))
  
  (def rdr (new LineNumberingPushbackReader (new InputStreamReader (. client (getInputStream)))))
  (def wtr (new OutputStreamWriter (. client (getOutputStream)))))

(comment
  (-main)
  

  )

(comment
  #_(binding [*out* wtr]
      (prn '(+ 1 2 3))
      (flush)
      (read rdr))
  
  (future
    (-> (binding [*out* wtr]
          (prn '(+ 5 5 5))
          (read rdr))
        println))
  
  (let [stdout *out*]
    (-> (binding [*out* wtr]
          (prn "a")
          (loop [e (read rdr)]
            (binding [*out* stdout]
              (println e))
            (flush)
            (recur (read rdr))))))
  
  (binding [*out* wtr]
    (println "a"))
  
  (def t (let [stdout *out*]
           (on-thread #(loop [e (.readLine rdr)]
                         (binding [*out* stdout]
                           (prn e))
                         (recur (read rdr))))))
  
  (comment
    (save :map-all)
    
    (loop [r (.readLine rdr)]
      (binding [*out* stdout]
        (pr r))
      (Thread/sleep 100)
      (recur (.readLine rdr))
      #_ (recur (read rdr false eof) (str res r)))
    )
  
  (. server (close))
  (. client (close))
  )
