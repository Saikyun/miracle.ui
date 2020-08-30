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

(defn hiccup
  [k width {:keys [filters pos height] :as state}]
  (let [saves (get @miracle.clj.save/saves k)
        ks (into #{} (mapcat keys saves))
        before 2
        after 2
        divider 3
        column-width (if (empty? ks)
                       width
                       (quot (- width
                                (* divider (dec (count ks)))
                                (+ before after))
                             (count ks)))
        remainder (if (empty? ks)
                    0
                    (rem (- width
                            (* divider (dec (count ks)))
                            (+ before after))
                         (count ks)))
        
        pos (max pos 0)
        
        form-lines [:div [:p k]
                    (into [:div {:display :grid}]
                          (for [k ks]
                            [:div k]))
                    (into [:div {:display :grid}]
                          (for [k ks]
                            [:input {:id (str "filter-" k)
                                     :value (or (get filters (str "filter-" k)) "")}]))]          
        
        filtered-bindings (filter
                           (fn [bindings]
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
                           saves)
        
        skip (quot pos 3) 

        filtered-bindings2 (drop (* 3 skip) filtered-bindings) 
        
        [new-pos filtered-bindings3] (if (empty? filtered-bindings2)
                                       [(max 0 (dec (count filtered-bindings)))
                                        (if-let [e (last filtered-bindings)]
                                          [e]
                                          [])]
                                       [pos (vec (take 10 filtered-bindings2))])
        
        save-lines (vec (for [bindings filtered-bindings3]
                          (do (save :huuh)
                              (into [:div {:display :grid, :pos pos, :skip skip, :selected
                                           (identical? bindings (get filtered-bindings3 (if (= 0 skip)
                                                                                          pos
                                                                                          (rem pos (* skip 3)))))}]
                                    (for [k ks
                                          :let [v (get bindings k)]]
                                      [:div (with-out-str (inspect-map v))])))))]
    (-> {:filter-line 4
         :save-name k
         :width width
         :filters filters
         :pos new-pos
         :selected-save (get filtered-bindings3 (if (= 0 skip)
                                                  pos
                                                  (rem pos (* skip 3))))
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

(def state (atom {:filters {}
                  :pos 0}))

(hiccup :map-all 80 @state)

(defn handle-input
  [in println]
  (save :in)
  (try
    (cond
      (= in 'a) (do (save :print)
                    (binding [*out* *out*]
                      (prn (:lines (hiccup :map-all 80 @state))))
                    (println "got command" in))
      
      (vector? in) (let [{:keys [value] :as command} (apply hash-map in)
                         new-state (swap! state assoc-in [:filters (:assoc command)] value)]
                     (println "printing" new-state)
                     #_(println (:lines (hiccup :map-all 80 new-state)))
                     (prn (:lines (hiccup :map-all 80 new-state)))
                     (println "done")
                     #_(println (:lines (hiccup :map-all 80 new-state))))
      
      (coll? in) (let [f (eval in)
                       new-state (swap! state #(hiccup :map-all 80 (f %)))]
                   (prn (:lines new-state)))
      
      :else
      (do (save :unreq)
          #_(prn "unreq" e)
          (println "unreeee" in)))
    (catch Exception e
      (save :err)
      (println "Got error: " e)
      (prn {:error (with-out-str (pprint (Throwable->map e)))})))
  (flush))

(defn print-all
  [ins outs]
  (let [stdout *out*]
    (binding [*out* (new OutputStreamWriter outs)]
      (let [r (new LineNumberingPushbackReader (new InputStreamReader ins))
            eof (Object.)]
        (loop [e (read r false eof)]
          (handle-input e #(binding [*out* stdout] (apply println %&)))
          (recur (read r false eof)))))))

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
  (doseq [i (range 100)
          :let [f (rand-nth [inc dec])
                v (vec (repeat 1000000 (rand-int 300000)))]]
    (miracle.playground/map-all f v))
  
  (start-ui-server!)
  
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
