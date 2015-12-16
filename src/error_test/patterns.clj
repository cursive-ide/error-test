(ns error-test.patterns)

(def patterns
  '{destructuring             (mark
                                (alt
                                  (symbol :not-matching & :into :targets)
                                  (vector
                                    (repeat invoke)
                                    (opt [(symbol :matching & :resolves? false)
                                          (capture (any :as :rest))
                                          invoke])
                                    (opt [(keyword :matching :as)
                                          (symbol :into :targets)]))
                                  (map
                                    (repeat
                                      (alt
                                        [(keyword :matching :keys)
                                         (vector
                                           (repeat
                                             (alt (symbol :into :targets)
                                                  (keyword :into :targets))))]
                                        [(alt (keyword :matching :strs)
                                              (keyword :matching :syms))
                                         (vector
                                           (repeat
                                             (symbol :into :targets)))]
                                        [(keyword :matching :or)
                                         (map
                                           (repeat [(symbol :into :or-symbols)
                                                    any]))]
                                        [(keyword :matching :as)
                                         (symbol :into :targets)]
                                        [(alt (keyword :into :targets)
                                              invoke)
                                         any])))))

    parameters-no-rest        (repeat
                                (capture
                                  (alt
                                    (symbol :not-matching & :into :parameters)
                                    (vector :into :parameters)
                                    (map :into :parameters)))
                                destructuring)

    parameters                [parameters-no-rest
                               (opt
                                 [(capture [(symbol :matching &) (alt (symbol :as :rest) (vector :as :rest) (map :as :rest))])
                                  (symbol :matching & :resolves? false)
                                  destructuring])]

    declare                   [symbol (repeat (symbol :into :name-symbols))]

    simple-def                [symbol (symbol :as :name-symbol) (opt (string :as :doc)) (any :as :value)]

    def-pattern               [symbol (symbol :as :name-symbol) (opt (string :as :doc)) (opt (any :as :value))]

    body                      (repeat
                                (any :into :body :called "body form"))

    arities                   (alt
                                (item :into :arities
                                      (vector :as :arglist :called "parameter vector" parameters) body)
                                (repeat+
                                  (item :into :arities
                                        (list :as :arity :called "multi-arity list"
                                              (vector :as :arglist :called "parameter vector" parameters)
                                              body))))

    defn-pattern              [symbol
                               (symbol :as :name-symbol :called "name symbol")
                               (opt (string :as :doc :called "docstring"))
                               (opt (map :as :param-map :called "attr map"))
                               arities]

    fn                        [symbol (opt (symbol :as :name-symbol)) arities]

    letfn                     [symbol (vector (repeat (item :into :functions (list :as :function (symbol :as :name-symbol) arities))))]

    local-binding             (vector
                                (repeat
                                  (item
                                    :into
                                    :bindings
                                    [(capture (alt (symbol :as :binding) (vector :as :binding) (map :as :binding)))
                                     destructuring
                                     (any :as :value)])))

    local-binding-form        [symbol local-binding body]

    for-bindings              (vector
                                (repeat
                                  (alt
                                    [(keyword :matching :let :into :modifiers) (item :into :sub-bindings local-binding)]
                                    [(alt (keyword :matching :when :into :modifiers) (keyword :matching :while :into :modifiers)) any]
                                    (item
                                      :into
                                      :bindings
                                      [(capture (alt (symbol :as :binding) (vector :as :binding) (map :as :binding)))
                                       destructuring
                                       (any :as :value)]))))

    for                       [symbol for-bindings]

    defmethod                 [symbol (symbol :as :multi-fn) (any :as :dispatch) arities]

    gen-class-clause          (repeat
                                (alt
                                  [(keyword :matching :name) (alt (string :as :fqn) (symbol :as :fqn :resolves? false))]
                                  [(keyword :matching :extends) (alt (string :as :extends) (symbol :as :extends))]
                                  [(keyword :matching :implements)
                                   (vector (repeat (alt (string :into :interfaces) (symbol :into :interfaces))))]
                                  [(keyword :matching :init) (alt string (symbol :resolves? false))]
                                  [(keyword :matching :constructors)
                                   (map
                                     (repeat
                                       (item
                                         :into
                                         :constructors
                                         [(vector
                                            :as
                                            :from-arglist
                                            (repeat (alt (string :into :from-params) (symbol :into :from-params))))
                                          (vector (repeat (alt (string :into :to-params) (symbol :into :to-params))))])))]
                                  [(keyword :matching :post-init) (alt string (symbol :resolves? false))]
                                  [(keyword :matching :methods)
                                   (vector
                                     (repeat
                                       (item
                                         :into
                                         :methods
                                         (vector
                                           :as
                                           :method
                                           (alt (string :as :name) (symbol :as :name :resolves? false))
                                           (vector (repeat (alt (string :into :parameters) (symbol :into :parameters))))
                                           (alt (string :as :return-type) (symbol :as :return-type))))))]
                                  [(keyword :matching :main) (any :as :main?)]
                                  [(keyword :matching :factory) (alt (string :as :factory) (symbol :as :factory :resolves? false))]
                                  [(keyword :matching :state) (alt (string :as :state) (symbol :as :state :resolves? false))]
                                  [(keyword :matching :exposes)
                                   (map
                                     (repeat
                                       (item
                                         :into
                                         :exposes
                                         (alt (string :as :field) (symbol :as :field :resolves? false))
                                         (map
                                           (repeat
                                             (alt
                                               [(keyword :matching :get) (alt (string :as :getter) (symbol :as :getter :resolves? false))]
                                               [(keyword :matching :set) (alt (string :as :setter) (symbol :as :setter :resolves? false))]))))))]
                                  [(keyword :matching :exposes-methods)
                                   (map
                                     (repeat
                                       (item
                                         :into
                                         :exposes-methods
                                         (alt (string :as :super-method) (symbol :as :super-method :resolves? false))
                                         (alt (string :as :method) (symbol :as :method :resolves? false)))))]
                                  [(keyword :matching :prefix) (alt string (symbol :resolves? false))]
                                  [(keyword :matching :impl-ns) (alt string (symbol :resolves? false))]
                                  [(keyword :matching :load-impl-ns) boolean]
                                  (item :into :unmatched (keyword :as :key) (any :as :value))))

    import-clause             (alt
                                (symbol :into :classes)
                                (item :into :packages
                                      (sequential :as :clause
                                                  (symbol :as :package)
                                                  (repeat+ (symbol :into :classes)))))

    refer-filters             (alt
                                [(keyword :matching :exclude) (sequential (repeat (symbol :into :exclude)))]
                                [(keyword :matching :rename) (map (repeat (symbol :into :rename-from) (symbol :into :rename-to)))]
                                [(keyword :matching :only) (sequential (repeat (symbol :into :only)))])

    require-filters           (alt
                                [(keyword :matching :as) (symbol :as :as)]
                                [(keyword :matching :refer)
                                 (alt (keyword :matching :all :as :refer-all) (sequential (repeat (symbol :into :refer))))])

    filter-clause             (mark
                                (item
                                  :into
                                  :clauses
                                  (alt
                                    (symbol :as :namespace)
                                    (sequential :as :spec (symbol :as :namespace) (repeat (alt require-filters refer-filters)))
                                    (item :as :prefix-spec (sequential :as :spec (symbol :as :prefix) (repeat invoke))))))

    require-flags             (alt
                                (keyword :matching :reload :into :flags)
                                (keyword :matching :reload-all :into :flags)
                                (keyword :matching :verbose :into :flags))

    require-clause            (alt filter-clause require-flags)

    use-clause                (alt filter-clause require-flags)

    require                   [symbol (repeat (quoted require-clause))]

    use                       [symbol (repeat (quoted use-clause))]

    import                    [symbol (repeat (alt (quoted import-clause) import-clause))]

    gen-class                 [symbol gen-class-clause]

    refer                     [symbol
                               (quoted (symbol :as :ns-name))
                               (repeat
                                 (alt
                                   [(keyword :matching :exclude) (quoted (sequential (repeat (symbol :into :exclude))))]
                                   [(keyword :matching :rename) (quoted (map (repeat (symbol :into :rename-from) (symbol :into :rename-to))))]
                                   [(keyword :matching :only) (quoted (sequential (repeat (symbol :into :only))))]))]

    ns-pattern                [symbol
                               (symbol :as :ns-name)
                               (opt (string :as :doc))
                               (opt map)
                               (repeat
                                 (alt
                                   (item
                                     :into
                                     :imports
                                     (sequential
                                       :as
                                       :import
                                       (alt (symbol :matching import) (keyword :matching :import))
                                       (repeat+ import-clause)))
                                   (item
                                     :into
                                     :requires
                                     (sequential
                                       :as
                                       :require
                                       (alt (symbol :matching require) (keyword :matching :require))
                                       (repeat+ require-clause)))
                                   (item
                                     :into
                                     :uses
                                     (sequential :as :use (alt (symbol :matching use) (keyword :matching :use)) (repeat+ use-clause)))
                                   (item
                                     :into
                                     :refers
                                     (sequential
                                       :as
                                       :refer
                                       (alt (symbol :matching refer) (keyword :matching :refer))
                                       (symbol :as :ns-name)
                                       (repeat refer-filters)))
                                   (item
                                     :into
                                     :refer-clojures
                                     (sequential
                                       :as
                                       :refer-clojure
                                       (alt (symbol :matching refer-clojure) (keyword :matching :refer-clojure))
                                       (repeat refer-filters)))
                                   (item
                                     :into
                                     :gen-classes
                                     (sequential
                                       :as
                                       :gen-class
                                       (alt (symbol :matching gen-class) (keyword :matching :gen-class))
                                       gen-class-clause))))]

    in-ns-pattern             [symbol (quoted (symbol :as :ns-name))]

    dot-form                  [symbol
                               (any :as :target)
                               (alt (list (any :as :member) (repeat (any :into :args))) [(any :as :member) (repeat (any :into :args))])]

    as-thread                 [symbol (any :as :expression) (symbol :as :binding) (repeat (any :into :forms))]

    cond-thread               [symbol (any :as :expression) (repeat [(any :into :conditions) (any :into :forms)])]

    defprotocol               [symbol
                               (symbol :as :name-symbol :resolves? false)
                               (opt (string :as :doc))
                               (repeat
                                 (item
                                   :into
                                   :methods
                                   (list
                                     :as
                                     :method
                                     (symbol :as :name-symbol :resolves? false)
                                     (repeat (item :into :arglists (vector :as :arglist parameters-no-rest)))
                                     (opt (string :as :doc)))))]

    type-methods              (repeat
                                (item
                                  :into
                                  :methods
                                  (list :as :method (symbol :as :name-symbol) (vector :as :arglist parameters-no-rest))))

    superclass-implementation (item
                                :into
                                :implementations
                                (symbol :as :name-symbol)
                                (vector :as :constructor-args)
                                type-methods)

    type-implementations      (repeat (item :into :implementations (symbol :as :name-symbol) type-methods))

    type-options              (repeat [(keyword :into :options) (any :into :option-values)])

    deftype                   [symbol
                               (symbol :as :name-symbol)
                               (vector :as :fields (repeat (symbol :into :field-symbols)))
                               type-options
                               type-implementations]

    defclass                  [symbol
                               (symbol :as :name-symbol)
                               (vector :as :fields (repeat (symbol :into :field-symbols)))
                               type-options
                               superclass-implementation
                               type-implementations]

    reify                     [symbol type-options type-implementations]

    extend-class              [symbol type-options superclass-implementation type-implementations]

    extend-type               [symbol
                               (alt (symbol :as :name-symbol) nil)
                               (repeat
                                 (item
                                   :into
                                   :implementations
                                   (symbol :as :name-symbol)
                                   (repeat (item :into :methods (list :as :method (symbol :as :name-symbol) arities)))))]

    extend-protocol           [symbol
                               (symbol :as :name-symbol)
                               (repeat
                                 (item
                                   :into
                                   :implementations
                                   (alt (symbol :as :name-symbol) nil)
                                   (repeat (item :into :methods (list :as :method (symbol :as :name-symbol) arities)))))]

    proxy                     [(symbol :as :proxy-symbol)
                               (vector :as :supers (repeat (symbol :into :super-symbols)))
                               (vector :as :super-params)
                               (repeat (item :into :methods (list :as :method (symbol :as :name-symbol) arities)))]

    definterface              [symbol
                               (symbol :as :name-symbol :resolves? false)
                               (repeat
                                 (item
                                   :into
                                   :methods
                                   (list
                                     :as
                                     :method
                                     (symbol :as :name-symbol :resolves? false)
                                     (vector :as :arglist (repeat (symbol :into :parameters :resolves? false))))))]

    condp                     [symbol
                               (any :as :pred)
                               (any :as :expr)
                               (repeat
                                 (item :into :clauses (any :as :expr) (alt [(keyword :matching :>>) (any :as :fn-value)] (any :as :value))))
                               (opt (any :as :default))]

    case                      [symbol (any :as :expr) (repeat (item :into :clauses (any :as :expr) (any :as :value))) (opt (any :as :default))]

    single-arg-form           [symbol any]
    })