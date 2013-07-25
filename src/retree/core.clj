(ns retree.core
  (:require [retree.protocols :as proto])
  (:refer-clojure :exclude (not repeat some iterate range while)))

;; The master plan:
;; v1: Function Combinators
;; v2: Interpretered DSL
;; v3: Compiled DSL

;; A strategy is a function of a tree that returns a rewritten tree, or nil.

;; TODO Consider traversals with side effects: Kiama's query & queryf
;; Also consider collect & other query-like things. Probably for another ns


;;; Conventions

; "t" stands for "Term" or "Tree"
; "s" stands for "Strategy"
; p/q/r are to f/g/h as strategies are to functions
; similar terms with t/u/v
;;TODO what does this "c" mean? Seems related to boundary strategies


;; Rewriting Context

(def ^:dynamic *rewriter*)


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

(defn pipe
  ([p q]
    (fn [t]
      (when-let [t* (p t)]
        (q t*))))
  ([p q & more]
    (apply pipe (pipe p q) more)))

(defn choice
  ([p q]
    (fn [t]
      (or (p t) (q t))))
  ([p q & more]
    (apply choice (choice p q) more)))

;;TODO non-deterministic choice (possible name: fork)

(defn branch [p q r]
  (fn [t]
    (if-let [t* (p t)]
      (q t*)
      (r t))))


;;; Repetition

(defn attempt [s]
  (choice s pass))

(defn repeat [s]
  (fn rec [t]
    ((attempt (pipe s rec)) t)))

;;TODO (repeat s c) (repeat s n)  -- need to rename one...

(defn repeat1 [s c]
  (fn rec [t]
    ((pipe s (choice rec c)) t)))

(defn repeat-until [s c]
  (fn rec [t]
    ((pipe s (choice c rec)) t)))

(defn while [test s]
  (fn rec [t]
    ((attempt (pipe test s rec)) t)))

(defn until [test s]
  (fn rec [t]
    ((choice (pipe s rec)) t)))

(defn do-while [s test]
  (pipe s (while test s)))

(defn iterate [init test advance]
  (pipe init (until test advance)))

(defn range [strategies from to]
  (if (<= from to)
    (pipe (strategies from) (range strategies (inc from) to))
    pass))

;;; Traversals

(defn composite? [x]
  (proto/-composite? *rewriter* x))

(defn at [key s]
  (fn [t]
    (when-let [v* (s (t key))]
      (assoc t key v*))))

;;TODO at-path

(defn all [s]
  (fn [t]
    (if (composite? t)
      (proto/-all *rewriter* t s)
      t)))

(defn all-td [s]
  (fn rec [t]
    ((choice s (all rec)) t)))

(defn all-bu [s]
  (fn rec [t]
    ((choice (all rec) s) t)))

(defn one [s]
  (fn [t]
    (if (composite? t)
      (proto/-one *rewriter* t s)
      t)))

(defn once-td [s]
  (fn rec [t]
    ((choice s (one rec)) t)))

(defn once-bu [s]
  (fn rec [t]
    ((choice (one rec) s) t)))

(defn some [s]
  (fn [t]
    (if (composite? t)
      (proto/-some *rewriter* t s)
      t)))

(defn some-td [s]
  (fn rec [t]
    ((choice s (some rec)) t)))

(defn some-bu [s]
  (fn rec [t]
    ((choice (some rec) s) t)))

;;TODO congruence -- works on vectors (and seqs?)
;; what about for maps? maybe it's like at-keys or something?

(defn breadth-first [s]
  (fn rec [t]
    ((pipe (all s) (all rec)))))

(defn top-down [s]
  (fn rec [t]
    ((pipe s (all rec)) t)))

;TODO (defn top-down-until [s test] ...)  ; topdownS

(defn bottom-up [s]
  (fn rec [t]
    ((pipe (all rec) s) t)))

;TODO bottom-up-until   ; bottomupS

(defn down-up
  ([s]
    (down-up s s))
  ([down up]
    (fn rec [t]
      ((pipe down (all rec) up) t))))

;TODO downup-until  ; downupS

(defn outermost [s]
  (repeat (once-td s)))

(defn innermost [s]
  (fn rec [t]
    ((bottom-up (attempt (pipe s rec))) t)))

;;TODO kiama reduce

;;TODO alldownup2
;;alltdfold
;;somedownup

(defn many-bu [s]
  (fn rec [t]
    ((choice (pipe s (attempt rec)) rec) t)))

(defn many-td [s]
  (fn rec [t]
    ((choice (pipe s (all (attempt rec))) (some rec)) t)))

;TODO kiama: eq/equal, issubterm, ispropersubterm issuperterm,
; ispropersuperterm, leaves, leaves, isinnernode

(defn everywhere-bu [s]
  (bottom-up (attempt s)))

(defn everywhere-td [s]
  (top-down (attempt s)))

(def everywhere everywhere-td)

;TODO kiama: everything, restore, restorealways, lastly, ior, or, and


;;; Debugging

(defn debug [s msg]
  (fn [t]
    (println msg)
    (s t)))

(defn trace [s]
  (fn [t]
    (println "Visiting: ")
    (prn t)
    (if-let [t* (s t)]
      (do
        (if (= t t*)
          (println "Matched!")
          (do
            (println "Rewritten to:")
            (prn t*)))
        t*)
      (do
        (println "Non-match:")
        (prn t)
        t))))

;;TODO trace-failures


;;; Predicates

(defn pred [f]
  (fn [t]
    (when (f t)
      t)))

;(defmacro defpred [name & ftail]
;  `(defn ~name (pred (fn ~@ftail))))

(def leaf?
  (all fail))

(defn not [s]
  (branch s fail pass))

(defn where [s]
  (fn [t]
    ((pipe s (build t)) t)))


;;; Execution

(defn rewrite [rewriter strategy tree]
  (binding [*rewriter* rewriter]
    (strategy tree)))


(comment

  (require '[retree.edn])
  (defn party [strategy term]
    (rewrite retree.edn/rewriter strategy term))

  (->> {}
    (party (pipe #(assoc % :x 1)
                 #(assoc % :y 2)
                 ;(constantly nil)
                 #(assoc % :z 3))))

  (->> {}
    (party (pipe #(assoc % :x 1)
                 #(assoc % :y 2)
                 ;(constantly nil)
                 #(assoc % :z 3))))

  (->> {:x -2}
    (party (choice #(when (= (:x %) 3)
                      (assoc % :y :three))
                   #(when (odd? (:x %))
                      (assoc % :y :odd))
                   #(when (pos? (:x %))
                      (assoc % :y :pos)))))

  (->> {:left {:value 1}
        :right {:left {:value 2}
                :right {:value 3}}}
    (party
      (repeat-until
        (all-td
          (branch (pred #(contains? % :value))
            #(update-in % [:value] inc)
            #(assoc % :interior true)))
        (pred #(contains? % :interior))))
    clojure.pprint/pprint)

  (->> {:left {:value 1}
        :right {:value 2}}
    (party
      (at :right
        #(update-in % [:value] inc))))

  (->> {:left {:value 1}
        :right {:value 2}}
    (party
      (choice
        (one (pipe (pred #(odd? (:value %))) ; also try even? and zero?
               #(update-in % [:value] inc)))
        #(assoc % :failed true))))

  (->> {:left {:value 1}
        :right {:value 2}}  ; also try both with 1, or both with 2
    (party
      (choice
        (some (pipe (pred #(odd? (:value %)))
                #(update-in % [:value] inc)))
        #(assoc % :failed true))))

  (->> {:left {:value 1}
        :right {:value 2}}
    (party (everywhere #(when (:value %)
                          (update-in % [:value] inc)))))

  (letfn [(parse [x]
            (if (seq? x)
              {:fn (first x) :arg (parse (second x))}
              x))
          (unparse [x]
            (if (map? x)
              (list (:fn x) (unparse (:arg x)))
              x))]
    (let [up-rules {'g #(when (= (:fn %) 'f)
                          {:fn 'g
                           :arg {:fn 'f
                                 :arg (-> % :arg :arg)}})}
          up-strategy #(when-let [f (and (:fn %) (-> % :arg :fn))]
                         (when-let [rule (up-rules f)]
                           (rule %)))]
    (->> (parse '(f (f (g 1))))
      (party (everywhere-bu up-strategy))
      unparse)))
  ;=> (g (f (f 1)))

  (->> [:plus [:plus 2 4] 6]
    (party (everywhere-bu
             #(when (and (vector? %) (= (first %) :plus))
                (apply + (rest %))))))

)
