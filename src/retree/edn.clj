(ns retree.edn
  (:require [retree.protocols :as proto :refer (IRewriter)]))

;;TODO Other composites besides maps & vectors?

(defn- kvps [term]
  (cond
    (map? term) term
    (vector? term) (map vector (clojure.core/range) term)))

(def rewriter
  (reify IRewriter

    (proto/-composite? [_ term]
      (or (map? term) (vector? term)))

    (proto/-all [_ term strategy]
      (reduce-kv (fn [t k v]
                   (if-let [v* (strategy v)]
                     (assoc t k v*)
                     (reduced nil)))
                 term
                 (kvps term)))

    (proto/-one [_ term strategy]
      (reduce (fn [t kvp]
                (when-not (= kvp :fail)
                  (let [[k v] kvp]
                    (if-let [v* (strategy v)]
                      (reduced (assoc t k v*))
                      t))))
              term
              (concat (kvps term) [:fail])))

    (proto/-some [_ term strategy]
      (let [successes (for [[k v] (kvps term)
                            :let [v* (strategy v)]
                            :when v*]
                        [k v*])]
        (when (seq successes)
          (reduce (partial apply assoc) term successes))))

    ))
