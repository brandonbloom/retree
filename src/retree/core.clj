(ns retree.core
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

(defn breadth-first [s]
  (fn rec [t]
    ((pipe (all s) (all rec)))))

(defn top-down [s]
  (fn rec [t]
    ((pipe s (all rec)) t)))

;TODO (defn top-down-until [s test] ...)  ; topdownS
;TODO bottom-up                           ; bottomup
;TODO bottom-up-until                     ; bottomupS
;TODO (downup s), (downup topdown-s bottomup-s)
;TODO downup-until                        ; downupS w/ 3 arg version too
;

;;TODO congruence -- works on vectors (and seqs?)
;; what about for maps? maybe it's like at-keys or something?


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

  (->> {}
    (rewrite (pipe #(assoc % :x 1)
                   #(assoc % :y 2)
                   ;(constantly nil)
                   #(assoc % :z 3))))

  (->> {}
    (rewrite (pipe #(assoc % :x 1)
                   #(assoc % :y 2)
                   ;(constantly nil)
                   #(assoc % :z 3))))

  (->> {:x -2}
    (rewrite (choice #(when (= (:x %) 3)
                        (assoc % :y :three))
                     #(when (odd? (:x %))
                        (assoc % :y :odd))
                     #(when (pos? (:x %))
                        (assoc % :y :pos)))))

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
        (one (pipe (pred #(odd? (:value %))) ; also try even? and zero?
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
