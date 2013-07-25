(ns retree.protocols)

(defprotocol IRewriter
  (-composite? [this term])
  (-all [this term strategy])
  (-one [this term strategy])
  (-some [this term strategy]))
