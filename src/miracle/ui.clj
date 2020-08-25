(ns miracle.ui
  (:require [miracle.clj.save :as s :refer [save ld]]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint pp]])
  (:import [java.net ServerSocket Socket SocketException]
           [java.io InputStreamReader OutputStreamWriter]
           [clojure.lang LineNumberingPushbackReader]))

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


(defn ascii
  [k width {:keys [filters]}]
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
        line (str/join "-+-" (for [k ks]
                               (apply str (repeat (if (= k (last ks))
                                                    (+ column-width remainder)
                                                    column-width)
                                                  "-"))))]
    {:filter-line 4
     :save-name k
     :width width
     :filters filters
     :ranges (-> (map-indexed (fn [i k] {:arg k
                                         :start (+ before (* i column-width))
                                         :end (+ before (* (inc i) column-width))})
                              ks)
                 vec)
     :lines (concat
             [k
              line 
              (str/join " | " (for [k ks]
                                (format (str "%-" (if (= k (last ks))
                                                    (+ column-width remainder)
                                                    column-width)
                                             "s") k)))
              line
              (str/join " | " (for [k ks]
                                (format (str "%-" (if (= k (last ks))
                                                    (+ column-width remainder)
                                                    column-width)
                                             "s") (or (get filters k) "<no filter>"))))
              line]
             (for [save (filter
                         (fn [bindings]
                           (if (seq filters)
                             (apply = true
                                    (for [[k pred] filters
                                          :let [str-filter #(str/includes? (str %) pred)]]
                                      (if (cond (str/starts-with? pred "#'")
                                                (try ((eval (read-string pred)) (get bindings k))
                                                     (catch Exception _
                                                       (str-filter (get bindings k))))
                                                
                                                (str/starts-with? pred "#")
                                                (try ((eval (read-string pred)) (get bindings k))
                                                     (catch Exception _
                                                       (str-filter (get bindings k))))
                                                
                                                :else
                                                (str-filter (get bindings k)))
                                        true
                                        nil)))
                             bindings))
                         saves)]
               (str (str/join
                     " | "
                     (for [k ks
                           :let [v (get save k)]]
                       (format (str "%-" (if (= k (last ks))
                                           (+ column-width remainder)
                                           column-width)
                                    "s") (apply str (take column-width (str v))))))))
             
             [line])}
    ))

(defn hiccup
  [k width {:keys [filters]}]
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
        line [:hr]]
    {:filter-line 4
     :save-name k
     :width width
     :filters filters
     :ranges (-> (map-indexed (fn [i k] {:arg k
                                         :start (+ before (* i column-width))
                                         :end (+ before (* (inc i) column-width))})
                              ks)
                 vec)
     :lines (vec
             (concat
              [k
               line 
               (vec (for [k ks]
                      [:div k]))
               line
               (vec (for [k ks]
                      [:div (or (get filters k) "<no filter>")]))
               line]
              (vec
               (for [save (filter
                           (fn [bindings]
                             (if (seq filters)
                               (apply = true
                                      (for [[k pred] filters
                                            :let [str-filter #(str/includes? (str %) pred)]]
                                        (if (cond (str/starts-with? pred "#'")
                                                  (try ((eval (read-string pred)) (get bindings k))
                                                       (catch Exception _
                                                         (str-filter (get bindings k))))
                                                  
                                                  (str/starts-with? pred "#")
                                                  (try ((eval (read-string pred)) (get bindings k))
                                                       (catch Exception _
                                                         (str-filter (get bindings k))))
                                                  
                                                  :else
                                                  (str-filter (get bindings k)))
                                          true
                                          nil)))
                               bindings))
                           saves)]
                 (vec (for [k ks
                            :let [v (get save k)]]
                        [:div (apply str (take column-width (str v)))]))))
              
              [line]))}
    ))

#_(let [{:keys [lines]} (ascii :map-all 80 {})]
    (println (str/join "\n" lines)))

(pprint (hiccup :map-all 80 {}))

(def state (ascii :map-all 80 {}))

(defn input
  [{:keys [filter-line ranges save-name width] :as state} {:keys [y x kind data]}]
  (if (= y filter-line)
    (let [{:keys [arg]} (first
                         (filter (fn [{:keys [start end]}]
                                   (and (<= start x)
                                        (< x end)))
                                 ranges))]
      (->> (update-in state [:filters arg] str data)
           (ascii save-name width)))
    state))

(comment
  (let [{:keys [lines]} (input state {:y 4 :x 3, :data "a"})]
    (println (str/join "\n" lines)))
  )

(comment
  (println (str/join "\n" (ascii :map-all 80 {:filters {'l "#'vector?"}})))

  (println (map meta (ascii :map-all 80 {:filters {'l "#'vector?"}}))))

(defn handle-input
  [e println]
  (if (= e "a")
    (do (save :print)
        (binding [*out* *out*]
          (s/print-saves :inc-all))
        (println "got command" e))
    (do (save :unreq)
        (prn "unreq" e)
        (println "unreeee" e)))
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
