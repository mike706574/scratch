(defn ns-key [ns k] (keyword (name ns) (name k)))

(def ns-key-here (partial ns-key (.getName *ns*)))

(defn walk-keys
  [f m]
  (letfn [(on-entry [[k v]]
            [(f k) v])]f
         (clojure.walk/postwalk (fn [x] (if (map? x) (into {} (map on-entry x)) x)) m)))

(defmacro with-metrics
  [& body]
  `(let [s# (new java.io.StringWriter)]
     (binding [*out* s#]
       (let [start# (System/nanoTime)
             out# ~(conj body `do)
             end# (System/nanoTime)]
         {:out out#
          :time (/ (- end# start#) 1000.0)
          :log (str s#)}))))

(defmacro with-time
  [& body]
  `(let [start# (System/nanoTime)
         out# ~(conj body `do)
         end# (System/nanoTime)]
     {:out out#
      :time (/ (- end# start#) 1000.0)}))

(defn process-config-file
  [file]
  (let [{:keys [out time]} (with-time
                             (let [id (.getName (io/file file))
                                   data (clojure.edn/read-string (slurp file))
                                   ns-data (walk-keys ns-key-here data)]
                               {:id id
;;                                :explanation (with-out-str (spec/explain ::release ns-data))
;;                                :explain-data (spec/explain-data ::release ns-data)
                                :valid? (spec/valid? ::thing ns-data)}
                               ))]
    (assoc out :time time)))

(process-thing-file "thing.edn")

(defn psplit
  [f xs]
  (reduce
   (fn [v x]
     (update-in v [(if (f x) 0 1)] #(conj % x)))
   [[] []]
   xs))

(defn mapm
  [f coll]
  (into {} (map f coll)))

(s/fdef mapm
  :args (s/cat :f fn? :coll map?)
  :ret map?
  :fn #(= (-> % :args :coll count) (-> % :ret count)))

(defn fmap
  [f m]
  (into {} (for [[k v] m] [k (f v)])))

(s/fdef fmap
  :args (s/cat :f fn? :m map?)
  :ret map?
  :fn (s/and #(= (-> % :args :coll count) (-> % :ret count))
             #(= (-> % :args :coll keys) (-> % :ret keys))))
