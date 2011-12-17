(ns dsann.utils.logic)

(defn between? [min max v]
  (and (< min v) (< v max)))

(defn between=? [min max v]
  (and (<= min v) (<= v max)))
