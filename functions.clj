(defn to-string
  [data]
  (with-out-str (cljs.pprint/pprint data)))
  
(defn includes-ignore-case?
  [string sub]
  (not (nil? (.match string (re-pattern (str "(?i)" sub))))))
