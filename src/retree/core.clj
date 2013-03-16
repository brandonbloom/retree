(ns retree.core
  (:refer-clojure :exclude (repeat)))

;; The master plan:
;; v1: Function Combinators
;; v2: Interpretered DSL
;; v3: Compiled DSL

;; A strategy is a function of a tree that returns a rewritten tree, or nil.

;; Consider traversals with side effects


;;; Conventions

; "t" stands for "Term" or "Tree"
; "s" stands for "Strategy"
; p/q/r are to f/g/h as strategies are to functions
; similar terms with t/u/v


;;; Primitive Strategies

(defn succeed [t] t)

(defn fail [t] nil)

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


;;; Core Strategies

(defn attempt [s]
  (choice s succeed))

(defn repeat [s]
  (fn rec [t]
    ((attempt (pipe s rec)) t)))

;; repeat is zero-or-more. Might be better named "iterate" to match Clojure
;; TODO repeat with terminal strategy, repeat-n, one-or-more
;; could also call these zom and oom



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
  (strategy tree))

(defn fixed-point [strategy tree]
  (let [tree* (strategy tree)]
    (cond
      (nil? tree*) tree
      (= tree tree*) tree* ;TODO: need this for metadata on tree* ?
      :else (recur strategy tree*))))


(comment

  (defn foo [{:keys [foo] :as t}]
    (when (< foo 5)
      (update-in t [:foo] inc)))

  (defn bar [t]
    (assoc t :bar 2))

  (->> {:foo 3}
    (rewrite
      (repeat foo)))

)
