(ns user)

(require '[error-test.parse :as parse]
         '[error-test.patterns :as patterns]
         '[clojure.string :as str])

(defn items-message [items]
  (case (count items)
    1 (first items)
    2 (str (first items) " or " (second items))
    (str (str/join ", " (butlast items))
         " or "
         (last items))))

(defn ^String message [state]
  (let [{:keys [::parse/error-offset ::parse/found ::parse/expected]} state]
    (if found
      (str "Unexpected "
           (cond
             (symbol? found) "symbol"
             (keyword? found) "keyword"
             (vector? found) "vector"
             (map? found) "map"
             (list? found) "list"
             (number? found) "number")
           (if (string? found)
             (str " \"" found "\"")
             (str " '" (pr-str found) "'"))
           (when expected
             (str ", expected " (items-message expected))))
      (str "Expected " (items-message expected)))))

(defn wrap! [var parser]
  (alter-var-root var (fn [original]
                        (fn [&form &env & args]
                          (let [state (parse/partial-parse &form parser)]
                            (if (or (::parse/error-offset state) (::parse/found state) (::parse/expected state))
                              (throw (IllegalArgumentException. (message state)))
                              (apply original &form &env args)))))))

(defn install-goodness []
  (wrap! #'defn (parse/compile-parser patterns/patterns 'defn-pattern))
  (wrap! #'ns (parse/compile-parser patterns/patterns 'ns-pattern)))

(comment
  (install-goodness)
  (defn x [] 10)
  (x)
  (defn a "asdf" ([a] 1) {:a :b} ([] 1))
  )
