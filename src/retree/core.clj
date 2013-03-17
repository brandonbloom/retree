(ns retree.core
  (:refer-clojure :exclude (repeat)))

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

(defn pred [f]
  (fn [t]
    (when (f t)
      t)))

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

(defn all [s]
  (fn [t]
    ;;TODO vectors & other collections
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
      (all-bu
        (branch (pred #(contains? % :value))
          #(update-in % [:value] inc)
          #(assoc % :interior true))))
    clojure.pprint/pprint)

)
