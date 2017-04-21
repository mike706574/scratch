(defmacro defp
  [sym v]
  `(do (def ~sym ~v)
       (def ~(build-symbol sym "-plus") ~(inc v))))

(defmacro deft
  [fn-symbol args & body]
  `(def ~fn-symbol
     (fn ~args
       (go
         (try
           (if (nil? (second ~args))
             {:status :bad-request :body "Type was nil!"}
             (if (type-exists? (first ~args) (second ~args))
               ~(conj body `do)
               {:status :missing :body (str "Invalid type ID: " (second ~args))}))
           (catch Exception e# {:status :error :body (.getMessage e#) :exception e#}))))))

(defmacro defm
  [fn-symbol & body]
  `(defn ~fn-symbol [id#]
     (pprint
      (let [~'repo (artifact/repo config)
            ~'id (name id#)]
        ~@body))))

(defp what 4)

(defn build-symbol
  [& args]
  (symbol (apply str (map name args))))


(defmacro defmonitor
  [sym]
  `(do
     (defonce ~sym (atom {}))
     (defn ~(build-symbol "monitor-" sym "!") [id#] (swap! ~sym add-monitor id#) nil)
     (defn ~(build-symbol "terminate-" sym "!") [id#] (swap! ~sym remove-monitor id#) nil)
     (defn ~(build-symbol "clear-" sym "!") [] (swap! ~sym clear-monitors) nil)
     (defn ~(build-symbol "active-" sym) [] (keys (deref ~sym)))))


(defmacro explode [& strings]
  `(throw (RuntimeException. ~(apply str strings))))

;; madness
(cons 4 [1 2 3])

(defmacro defjam
  [fn-symbol args & body]
  `(defn ~fn-symbol ~(into [] (cons ~id args))
     (let [~'gitlab-id (get-gitlab-id id)]
       ~@body)))

(macroexpand
 (defjam hi-mom
  [yo-dawg]
  (println id)
  (println gitlab-id)
  (println yo-dawg)))

(defmacro pull [n m] `(def n (get ~m ~(keyword n))))

(defmacro validate
    [& body]
    {:pre [(odd? (count body))]}
    (let [pairs (partition 2 body)
                  default (last body)]
          (concat
                 [`cond]
                 (apply concat
                        (for [[condition message] pairs]
                          [condition {:status :bad :message message}]))
                 [:else default])))


(defmacro defaliases
  [& pairs]
  (let [args (gensym 'args)]
    `(do
       ~@(for [[action-name f] (partition 2 pairs)]
           `(defmethod run ~action-name
              [~(symbol "action") & ~args]
              (apply ~f ~args))))))

  (defmacro cfn
    [arg-list & body]
    `(with-meta
       (fn ~arg-list
         ~(conj body `do))
       {:args ~(mapv name arg-list)}))

  (defmacro defcmd
    [symbol arg-list & body]
    `(def ~symbol
       (with-meta
         (fn ~arg-list
           ~(conj body `do))
         {:args ~(mapv name (rest arg-list))})))


(defmacro assoc-when
  [test map & kvs]
  `(if ~test
     (apply assoc ~map ~(vec kvs))
     ~map))

(defmacro when-ok
    [response & fn-body]
  `(when (= (:status ~response) :ok)
     (let [~'body (:body ~response)]
        ~(conj fn-body `do))
      (throw (ex-info status ~response))))

 (when-ok {:status :foo}
   (println body))


(defn ns-key [ns k] (keyword (name ns) (name k)))

(def ns-key-here (partial ns-key (.getName *ns*)))

(defn walk-keys
  [f m]
  (letfn [(on-entry [[k v]]
            [(f k) v])]f
         (clojure.walk/postwalk (fn [x] (if (map? x) (into {} (map on-entry x)) x)) m)))

(defmacro with-log
  [& body]
  `(let [s# (new java.io.StringWriter)]
     (binding [*out* s#]
       {:out ~@body :log (str s#)})))

(defmacro debug-time
  [message & body]
  `(let [start# (System/nanoTime)
         out# ~(conj body `do)
         end# (System/nanoTime)]
     (log/debug (str ~message " " (/ (- end# start#) 1000.0) " ms."))
     out#))

(defmacro defcatch
  [fn-symbol args & body]
  `(def ~fn-symbol
     (fn ~args
       (try
         ~(conj body `do)
         (catch Exception e#
           {:status :error
            :message (.getMessage e#)})))))

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

(defmacro if-not-let
  ([bindings then]
   `(if-let ~bindings ~then nil))
  ([bindings then else]
   (let [form (bindings 0) tst (bindings 1)]
     `(let [temp# ~tst]
        (if-not temp#
          ~then
          (let [~form temp#]
            ~else))))))

(defmacro with-body
  [[body-sym body-spec request] & body]
  `(or (unsupported-media-type ~request)
       (not-acceptable ~request)
       (let [~body-sym (parsed-body ~request)]
          (if-not ~body-sym
            (body-response 400 ~request {:milo.server/message "Invalid request body representation."})
            (if-let [validation-failure# (spec/explain-data ~body-spec ~body-sym)]
              (body-response 400 ~request {:milo.server/message "Invalid request body."
                                           :milo.server/data validation-failure#})
              ~@body)))))

(defmacro handle-exceptions
  [request & body]
  `(try
     ~@body
     (catch Exception e#
       (log/error e# "An exception was thrown while processing a request.")
       (body-response 500 ~request {:milo.server/message "An error occurred."}))))

(defmacro is#
  [form msg & body]
  `(let [result# ~form]
     (clojure.test/do-report
      {:actual result#
       :expected '~form
       :message ~msg
       :type (if result# :pass :fail)})
     (when result#
       ~@body)))
