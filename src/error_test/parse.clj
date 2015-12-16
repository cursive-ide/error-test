(ns error-test.parse
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [error-test.patterns :as patterns]
            [clojure.tools.reader :as r])
  (:import (clojure.lang IMeta)))


(def error-keys [::error-progress ::expected ::found])
(def failure-keys (conj error-keys ::remaining))

(defn failed? [state]
  (= ::fail (::remaining state)))

(defn compare-progress [progress1 progress2]
  (if-let [item1 (first progress1)]
    (if-let [item2 (first progress2)]
      (cond
        (= item1 item2) (recur (rest progress1) (rest progress2))
        (= item1 :in) -1
        :else 1)
      1)
    (if (first progress2) -1 0)))

(defn copy-values [map1 map2 keys]
  (reduce (fn [ret key]
            (if (contains? map2 key)
              (assoc ret key (get map2 key))
              (dissoc ret key)))
          map1
          keys))

(defn join-errors
  "Returns state1 with its error information merged with that of state2"
  [state1 state2]
  (let [offset1 (::error-progress state1)
        offset2 (::error-progress state2)]
    (cond
      (and offset1 offset2)
      (case (compare-progress offset1 offset2)
        0 (assoc state1 ::expected (set/union (::expected state1) (::expected state2)))
        -1 (copy-values state1 state2 error-keys)
        state1)
      offset1 state1
      offset2 (copy-values state1 state2 error-keys)
      :else state1)))

(defn concatenation [state parsers]
  (loop [current state
         parsers parsers]
    (if-not (seq parsers)
      current
      (let [parser (first parsers)]
        (let [new-state (parser current)]
          (if (failed? new-state)
            new-state                                       ; con.2
            (recur (join-errors new-state current) (rest parsers)))))))) ; con.1

(defn gather-params [args]
  (loop [params {}
         parsers args]
    (if (keyword? (first parsers))
      (recur (assoc params (first parsers) (second parsers)) (drop 2 parsers))
      [params parsers])))

(defn only-params [args]
  (let [[params parsers] (gather-params args)]
    (if (seq parsers)
      (throw (IllegalArgumentException. "Parser does not accept sub-parsers"))
      params)))

(defn process-terminal [{:keys [::remaining] :as state} predicate description]
  (if-let [item (first remaining)]
    (if (predicate item)
      [state item (rest remaining)] ; char.1
      (let [{:keys [::error-progress ::progress]} state]    ; char.2
        (cond
          (or (nil? error-progress)
              (pos? (compare-progress progress error-progress)))
          [(assoc state ::remaining ::fail
                        ::error-progress progress
                        ::expected #{description}
                        ::found item)]
          (zero? (compare-progress progress error-progress))
          [(assoc state ::remaining ::fail
                        ::expected (conj (::expected state) description))]
          :else [(assoc state ::remaining ::fail)])))
    (let [{:keys [::error-progress ::progress]} state]    ; char.3
      (cond
        (or (nil? error-progress)
            (pos? (compare-progress progress error-progress)))
        [(assoc (dissoc state ::found)
           ::remaining ::fail
           ::error-progress progress
           ::expected #{description})]
        (zero? (compare-progress progress error-progress))
        [(assoc (dissoc state ::found)
           ::remaining ::fail
           ::expected (conj (::expected state) description))]
        :else [(assoc state ::remaining ::fail)]))))

(defn symbol-parser [params]
  (let [{:keys [as into called matching not-matching resolves?]} params
        resolves? (or (nil? resolves?) resolves?)
        predicate (cond
                    matching (let [match (name matching)]
                               #(and (symbol? %) (= match (name %))))
                    not-matching (let [match (name not-matching)]
                                   #(and (symbol? %) (not= match (name %))))
                    :else symbol?)]
    (fn [state]
      (let [[state item remaining] (process-terminal state predicate (or called
                                                                         (and matching (name matching))
                                                                         "symbol"))]
        (if item
          (cond-> (-> (update state ::progress conj :next)
                      (assoc ::remaining remaining))
            (not resolves?) (assoc ::no-resolves (conj (get state ::no-resolves []) item))
            as (assoc as item)
            into (assoc into (conj (get state into []) item)))
          state)))))

(defn keyword-parser [params]
  (let [{:keys [as into called matching]} params
        predicate (if matching
                    (let [match (name matching)]
                      #(and (keyword? %) (= match (name %))))
                    keyword?)]
    (fn [state]
      (let [[state item remaining] (process-terminal state predicate (or called
                                                                         (and matching (str \: (name matching)))
                                                                         "keyword"))]
        (if item
          (cond-> (-> (update state ::progress conj :next)
                      (assoc ::remaining remaining))
            as (assoc as item)
            into (assoc into (conj (get state into []) item)))
          state)))))

(defn check-end [state]
  (let [{:keys [::remaining ::progress]} state]
    (let [item (when-not (keyword? remaining) (first remaining))]
      (if item
        (join-errors (assoc state ::remaining ::fail)
                     {::error-progress progress
                      ::found          item
                      ::expected       #{"end of form"}})
        state))))

(defn parser-from
  ([predicate type params parsers]
   (parser-from predicate type params parsers identity))
  ([predicate type params parsers calculate-rest]
   (let [{:keys [as into called]} params]
     (fn [state]
       (let [[state item remaining] (process-terminal state predicate (or called (str/replace (name type) \- \space)))]
         (if item
           (let [state (cond-> state
                         as (assoc as item)
                         into (assoc into (conj (get state into []) item)))]
             (if (seq parsers)
               (let [next-state (assoc (update state ::progress conj :in)
                                  ::remaining (calculate-rest item))
                     inner (check-end (concatenation next-state parsers))]
                 (if (failed? inner)
                   (assoc inner ::progress (::progress state))
                   (assoc inner ::remaining remaining
                                ::progress (conj (::progress state) :next))))
               (-> (update state ::progress conj :next)
                   (assoc ::remaining remaining))))
           state))))))

(defn single-parser-from
  [predicate type params]
  (let [{:keys [as into called]} params]
    (fn [state]
      (let [[state item remaining] (process-terminal state predicate (or called (str/replace (name type) \- \space)))]
        (if item
          (cond-> (-> (update state ::progress conj :next)
                      (assoc ::remaining remaining))
            as (assoc as item)
            into (assoc into (conj (get state into []) item)))
          state)))))

(defn all-parser [parsers]
  (fn [state] (concatenation state parsers)))

(defn alt-parser [parsers]
  (fn [state]
    (loop [parsers parsers
           current state]
      ; TODO think about this - it's confusing in the case of partial parses
      (if-let [parser (first parsers)]
        (let [new-state (parser current)]
          (if (failed? new-state)
            (recur (rest parsers) (join-errors current new-state))
            (join-errors new-state current)))
        (assoc current ::remaining ::fail)))))

(defn opt-parser [parsers]
  (fn [state]
    (let [new-state (concatenation state parsers)]
      (if (failed? new-state)
        (join-errors state new-state)
        (join-errors new-state state)))))

(defn repeat-parser [parsers]
  (fn [state]
    (let [new-state (concatenation state parsers)]
      (if (failed? new-state)
        (join-errors state new-state)
        (recur (join-errors new-state state))))))

(defn one-or-more-parser [parsers]
  (all-parser (concat parsers [(repeat-parser parsers)])))

(defn item-parser [params parsers]
  (let [{:keys [as into]} params]
    (fn [state]
      (let [new-state (concatenation (select-keys state [::remaining ::progress ::mark]) parsers)]
        (if (failed? new-state)
          (join-errors (assoc state ::remaining ::fail) new-state)
          (let [item (dissoc new-state ::remaining ::progress ::mark ::no-resolves ::expected ::found ::error-progress)
                ret (join-errors state new-state)
                ret (cond
                      as (assoc ret
                           as item
                           ::remaining (::remaining new-state)
                           ::progress (::progress new-state))
                      into (assoc ret
                             into (conj (get ret into []) item)
                             ::remaining (::remaining new-state)
                             ::progress (::progress new-state))
                      :else ret)]                           ; :else clause should probably be an error
            (if-let [no-resolves (::no-resolves new-state)]
              (assoc ret ::no-resolves (into (get ret ::no-resolves []) no-resolves))
              ret)))))))

(defn mark-parser [parser]
  (fn [state]
    (let [new-state (parser (assoc state ::mark parser))]
      (if-let [old-mark (::mark state)]
        (assoc new-state ::mark old-mark)
        (dissoc new-state ::mark)))))

(defn invoke-parser []
  (fn [state]
    (let [parser (::mark state)]
      (parser state))))

(defn capture-parser
  "This is a capturing lookahead - values captured by the :as or :into clauses
   in parsers will remain in the state."
  [parsers]
  (fn [state]
    (let [new-state (concatenation state parsers)]
      (if (failed? new-state)
        (copy-values state new-state failure-keys)
        (assoc new-state ::remaining (::remaining state))))))

(defn error [form ^String message]
  (if (instance? IMeta form)
    (let [{:keys [file line]} (meta form)]
      (if (and file line)
        (throw (IllegalArgumentException. (str message " (" file ":" line ")")))
        (throw (IllegalArgumentException. message))))
    (throw (IllegalArgumentException. message))))

(defn create-parser [head params parsers]
  (when (and (#{'symbol 'keyword 'string 'any 'boolean 'invoke nil} head)
             (seq parsers))
    (error head (str head " does not accept sub-parsers")))

  (when (and (#{'item 'alt 'opt 'repeat 'repeat+ 'capture} head)
             (empty? parsers))
    (error head (str head " requires sub-parsers")))

  (when (and (#{'quoted 'mark} head)
             (not= 1 (count parsers)))
    (error head (str head " requires exactly one sub-parser")))

  (when (and (#{'alt 'opt 'repeat 'repeat+ 'capture 'mark 'invoke} head)
             (not (empty? params)))
    (error head (str head " does not accept params (given " params ")")))

  (case head
    nil (single-parser-from nil? :nil nil)
    symbol (symbol-parser params)
    keyword (keyword-parser params)
    string (single-parser-from string? :string params)
    any (single-parser-from identity :form params)
    boolean (single-parser-from #(or (true? %) (false? %)) :boolean params)
    list (parser-from list? :list params parsers)
    vector (parser-from vector? :vector params parsers)
    sequential (parser-from sequential? :sequential-form params parsers)
    map (parser-from map? :map params parsers #(mapcat identity %))
    quoted (parser-from list? :list params
                        (cons (symbol-parser [:matching 'quote])
                              parsers))
    item (item-parser params parsers)
    alt (alt-parser parsers)
    opt (opt-parser parsers)
    repeat (repeat-parser parsers)
    repeat+ (one-or-more-parser parsers)
    capture (capture-parser parsers)
    mark (mark-parser (first parsers))
    invoke (invoke-parser)
    nil))

(defn compile-parser [patterns pattern]
  (or
    (cond
      (nil? pattern) (create-parser pattern nil nil)
      (symbol? pattern) (or (create-parser pattern nil nil)
                            (if-let [pattern (get patterns pattern)]
                              (compile-parser patterns pattern)
                              (error pattern (str "No rule found for " pattern))))
      (list? pattern) (let [head (first pattern)
                            [params sub-parsers] (gather-params (rest pattern))
                            sub-parsers (map #(compile-parser patterns %) sub-parsers)]
                        (or (create-parser head params sub-parsers)
                            (error pattern (str "List rules can only start with terminal pattern " pattern))))
      (vector? pattern) (all-parser (map #(compile-parser patterns %) pattern))
      :else nil)
    (error pattern (str "Can't create parser from " pattern))))

(defn tidy [state]
  (if (failed? state)
    (dissoc state ::remaining ::progress)
    (dissoc state ::remaining ::progress ::error-progress ::expected ::found)))

(defn partial-parse [form parser]
  (tidy
    (check-end
      (parser {::remaining form
               ::progress  []}))))

(defn parse [form parser]
  (let [state (partial-parse form parser)]
    (when-not (or (::error-progress state) (::found state) (::expected state))
      state)))
