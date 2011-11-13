(ns utils.color.pallet
  (:require 
    [utils.color.protocols :as cp]
    [utils.color.fns :as ucf]
    [utils.x.core :as u]
    )
  )


(def red   (cp/make-rgb 255 0 0))
(def green (cp/make-rgb 0 255 0))
(def blue  (cp/make-rgb 0 0 255))
(def black (cp/make-rgb 0))
(def white (cp/make-rgb 255))

(defn color-name [color-set index]
  (str (name color-set) "-" index))


(defn color-range [adjust-fn base-color num-colors]
  (map #(adjust-fn base-color * %) (range 1 0 (/ -1 num-colors))))

(defn rgb-color-range [adjust-fn base-color num-colors]
  (map #(adjust-fn base-color + %) (range 255 0 (/ -256 num-colors))))


(defn l-color-range [base-color num-colors]
  (color-range ucf/lighten base-color num-colors))

(defn s-color-range [base-color num-colors]
  (color-range ucf/saturate base-color num-colors))

(defn r-color-range [base-color num-colors]
  (rgb-color-range ucf/redify base-color num-colors))

(defn g-color-range [base-color num-colors]
  (rgb-color-range ucf/greenify base-color num-colors))

(defn b-color-range [base-color num-colors]
  (rgb-color-range ucf/blueify base-color num-colors))

(defn h-color-range [base-color num-colors]
  (map #(ucf/rotate base-color + %) (range 0 360 (/ 360 num-colors))))


(defn triadic-harmony [color]
  [(ucf/rotate color + 120) (ucf/rotate color - 120)])

(defn split-complements [color]
  [(ucf/rotate color + 150) (ucf/rotate color - 150)])

(defn analoagous-colors [color]
  [(ucf/rotate color + 30) (ucf/rotate color - 30)])

(defn complement-color [color]
  (ucf/rotate color + 180))


(defn mono-pallet [base-color color-seq]
  {:primary (color-seq base-color)})

(defn complement-pallet [base-color color-seq]
  (let [comp-color (ucf/rotate base-color + 180)]
    {:primary    (color-seq base-color)
     :complement (color-seq comp-color)}
    ))

(defn triad-pallet [base-color color-seq angle]
  {:primary      (color-seq base-color)
   :secondary-1  (color-seq (ucf/rotate base-color + (- 180 angle)))
   :secondary-2  (color-seq (ucf/rotate base-color + (+ 180 angle)))}
  )

(defn tetrad-pallet [base-color color-seq angle]
  {:primary      (color-seq base-color)
   :secondary-1  (color-seq (ucf/rotate base-color + angle))
   :complement   (color-seq (ucf/rotate base-color + 180))
   :secondary-2  (color-seq (ucf/rotate base-color + (+ 180 angle)))
   }
  )

(defn pallet-2d [base-color color-seq1 color-seq2 num-colors]
  (into (sorted-map)
        (map-indexed 
          (fn [i color]
            [(format "c%03d" i) (color-seq2 color num-colors)])
          (color-seq1 base-color num-colors))))


