(ns clojure.core.matrix.macros
  "Namespace for core.matrix macros. Keeping them separate allows us to do conditional
  macros that can handle the differences between Clojure and Clojurescript."
  (:refer-clojure :exclude [array?]))

(defn- cljs-env?
  "Take the &env from a macro, and tell whether we are expanding into cljs."
  [env]
  (boolean (:ns env)))

(defmacro if-cljs
  "Return then if we are generating cljs code and else for Clojure code.
   https://groups.google.com/d/msg/clojurescript/iBY5HaQda4A/w1lAQi9_AwsJ"
  [then else]
  (if (cljs-env? &env) then else))

(defmacro error
  "Throws an error with the provided message(s)"
  ([& vals]
   `(if-cljs
      (throw (js/Error. (str ~@vals)))
      (throw (RuntimeException. (str ~@vals))))))

(defmacro error?
  "Returns true if executing body throws an error, false otherwise."
  ([& body]
    `(if-cljs
       (try
         ~@body
         false
         (catch js/Error t#
           true))
       (try
         ~@body
         false
         (catch Throwable t#
           true)))))

;; useful TODO macro: facilitates searching for TODO while throwing an error at runtime :-)
(defmacro TODO
  ([] `(error "TODO: not yet implemented"))
  ([& vals] `(error "TODO: " ~@vals)))

(defmacro iae
  "Throws IllegalArgumentException with provided string"
  [exception-str]
  `(throw (IllegalArgumentException. ~exception-str)))

(defmacro iae-when-not
  "Throws an IllegalArgumentException when the predicate is not satisfied"
  [pred? exception-str]
  `(when-not ~pred?
     (iae ~exception-str)))

(defmacro doseq-indexed
  "loops over a set of values, binding index-sym to the 0-based index of each value"
  ([[val-sym values index-sym] & code]
  `(loop [vals# (seq ~values)
          ~index-sym (long 0)]
     (if vals#
       (let [~val-sym (first vals#)]
             ~@code
             (recur (next vals#) (inc ~index-sym)))
       nil))))

#?(:clj (def object-array-class (Class/forName "[Ljava.lang.Object;")))

(defn is-object-array? [m]
  #?(:clj (instance? object-array-class m)
     :cljs (= js/Array (type m))))

#?(:clj (def long-array-class (Class/forName "[J")))

(defn is-long-array? [m]
  #?(:clj (instance? long-array-class m)
     :cljs (= js/Array (type m))))

#?(:clj (def double-array-class (Class/forName "[D")))

(defn is-double-array? [m]
  #?(:clj (instance? double-array-class m)
     :cljs (= js/Array (type m))))

(defmacro c-for
  "C-like loop with nested loops support"
  [loops & body]
  (letfn [(c-for-rec [loops body-stmts]
            (if (seq loops)
              (let [[var init check next] (take 4 loops)]
                `((loop [~var ~init]
                     (when ~check
                       ~@(c-for-rec (nthrest loops 4) body-stmts)
                       (recur ~next)))))
              body-stmts))]
    `(do ~@(c-for-rec loops body) nil)))

(defmacro scalar-coerce
  "Macro to coerce to scalar value with an efficient dispatch sequence"
  ([x]
  `(let [x# ~x]
     (cond
       (number? x#) x#
       :else (clojure.core.matrix.protocols/get-0d x#)))))

(defmacro array?
  "Returns true if the parameter is an N-dimensional array of any type"
  ([m]
    `(not (mp/is-scalar? ~m))))

