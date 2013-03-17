(ns retree.core
  (:refer-clojure :exclude (repeat some)))

;; The master plan:
;; v1: Function Combinators
;; v2: Interpretered DSL
;; v3: Compiled DSL

;; A strategy is a function of a tree that returns a rewritten tree, or nil.

;; Consider traversals with side effects: Kiama's query & queryf


;;; Conventions

; "t" stands for "Term" or "Tree"
; "s" stands for "Strategy"
; p/q/r are to f/g/h as strategies are to functions
; similar terms with t/u/v
;;TODO what does this "c" mean? Seems related to boundary strategies


;;; Primitive Strategies

(defn pass [t] t)

(defn fail [t] nil)

(defn build [t]
  (fn [_]
    t))

(defn term [t]
  (fn [u]
    (when (= t u)
      t)))

;;TODO variadic pipe & choice

(defn pipe [p q]
  (fn [t]
    (when-let [t* (p t)]
      (q t*))))

;; This is ordered or "determinisitic choice
;;TODO: Consider non-deterministic choice (possible name: fork)
(defn choice [p q]
  (fn [t]
    (or (p t) (q t))))

(defn branch [p q r]
  (fn [t]
    (if-let [t* (p t)]
      (q t*)
      (r t))))


;;; Predicates

(defn pred [f]
  (fn [t]
    (when (f t)
      t)))

(def leaf?
  (all fail))


;;; Core Strategies

(defn attempt [s]
  (choice s pass))

(defn repeat [s]
  (fn rec [t]
    ((attempt (pipe s rec)) t)))

(defn repeat1 [s c]
  (fn rec [t]
    ((pipe s (choice rec c)) t)))

(defn repeat-until [s c]
  (fn rec [t]
    ((pipe s (choice c rec)) t)))


;;; Traversals

(defn at [key s]
  (fn [t]
    (when-let [v* (s (t key))]
      (assoc t key v*))))

;;TODO at-path

(defn all [s]
  (fn [t]
    ;;TODO vectors & other collections (see also: one, some, etc)
    (if (map? t)
      (reduce-kv (fn [t* k v]
                   (if-let [v* (s v)]
                     (assoc t* k v*)
                     (reduced nil)))
                 t t)
      t)))

(defn all-td [s]
  (fn rec [t]
    ((choice s (all rec)) t)))

(defn all-bu [s]
  (fn rec [t]
    ((choice (all rec) s) t)))

(defn one [s]
  (fn [t]
    (when (map? t)
      (reduce (fn [t* kvp]
                (when-not (= kvp :fail)
                  (let [[k v] kvp]
                    (if-let [v* (s v)]
                      (reduced (assoc t* k v*))
                      t*))))
              t
              (concat t [:fail])))))

(defn once-td [s]
  (fn rec [t]
    ((choice s (one rec)) t)))

(defn once-bu [s]
  (fn rec [t]
    ((choice (one rec) s) t)))

(defn some [s]
  (fn [t]
    (when (map? t)
      (let [successes (for [[k v] t
                            :let [v* (s v)]
                            :when v*]
                        [k v*])]
        (when (seq successes)
          (into t successes))))))

(defn some-td [s]
  (fn rec [t]
    ((choice s (some rec)) t)))

(defn some-bu [s]
  (fn rec [t]
    ((choice (some rec) s) t)))


;;; Debugging

(defn debug [s msg]
  (fn [t]
    (println msg)
    (s t)))

(defn trace [s]
  (fn [t]
    (if-let [t* (s t)]
      (do
        (if (= t t*)
          (do (println "Match:") (prn t*))
          (do (println "Rewriting:") (prn t) (println "To:") (prn t*)))
        t*)
      (do
        (println "Non-match:") (prn t)
        t))))

;;TODO trace-failures


;;; Execution

(defn rewrite [strategy tree]
  (or (strategy tree) tree))

;;TODO this should be a strategy combinator itself
(defn fixed-point [strategy tree]
  (let [tree* (strategy tree)]
    (cond
      (nil? tree*) tree
      (= tree tree*) tree*
      :else (recur strategy tree*))))


(comment

  (->> {:left {:value 1}
        :right {:left {:value 2}
                :right {:value 3}}}
    (rewrite
      (repeat-until
        (all-td
          (branch (pred #(contains? % :value))
            #(update-in % [:value] inc)
            #(assoc % :interior true)))
        (pred #(contains? % :interior))))
    clojure.pprint/pprint)

  (->> {:left {:value 1}
        :right {:value 2}}
    (rewrite
      (at :right
        #(update-in % [:value] inc))))

  (->> {:left {:value 1}
        :right {:value 2}}
    (rewrite
      (choice
        (one (pipe (pred #(odd? (:value %))) ; also tree even? and zero?
               #(update-in % [:value] inc)))
        #(assoc % :failed true))))

  (->> {:left {:value 1}
        :right {:value 2}}  ; also try both with 1, or both with 2
    (rewrite
      (choice
        (some (pipe (pred #(odd? (:value %)))
                #(update-in % [:value] inc)))
        #(assoc % :failed true))))


)
