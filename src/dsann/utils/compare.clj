(ns dsann.utils.compare)

(defn between? [min max v]
  (and (< min v) (< v max)))

(defn between=? [min max v]
  (and (<= min v) (<= v max)))
