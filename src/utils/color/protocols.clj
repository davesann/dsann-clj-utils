(ns utils.color.protocols
  (:require 
    [utils.x.core :as u]
    [utils.map  :as um]
    [utils.math :as umath]
    [utils.color.protocol-utils :as cpu]
    [utils.color.conversion :as cc]
    )
  )

(defprotocol ColorConversion  
  (->rgb  [this] "convert to rgba")
  (->hsl  [this] "convert to hsla")
  (->cmyk [this] "convert to cmyk")
  (->cmy  [this] "convert to cmyk")
  )

(declare make-rgb)
(declare make-hsl)
(declare make-cmy)
(declare make-cmyk)

(defrecord RGBa [red green blue alpha]
  ColorConversion
  (->rgb  [this] this)
  (->hsl  [this] (cpu/convert-rgb this cc/rgb->hsl  make-hsl ))
  (->cmyk [this] (cpu/convert-rgb this cc/rgb->cmyk make-cmyk))
  (->cmy  [this] (cpu/convert-rgb this cc/rgb->cmy  make-cmy ))
  )

(defrecord HSLa [hue saturation lightness alpha]
  ColorConversion
  (->rgb  [this] (cpu/convert-hsl this cc/hsl->rgb  make-rgb))
  (->hsl  [this] this)
  (->cmyk [this] (cpu/convert-hsl this cc/hsl->cmyk make-cmyk))
  (->cmy  [this] (cpu/convert-hsl this cc/hsl->cmy  make-cmy ))
  )

(defrecord CMYa [cyan magenta yellow alpha]
  ColorConversion
  (->rgb  [this] (cpu/convert-cmy this cc/cmy->rgb  make-rgb))
  (->hsl  [this] (cpu/convert-cmy this cc/cmy->hsl  make-hsl ))
  (->cmyk [this] (cpu/convert-cmy this cc/cmy->cmyk make-cmyk))
  (->cmy  [this] this)
 )

(defrecord CMYKa [cyan magenta yellow k alpha]
  ColorConversion
  (->rgb  [this] (cpu/convert-cmyk this cc/cmyk->rgb make-rgb))
  (->hsl  [this] (cpu/convert-cmyk this cc/cmyk->hsl make-hsl)) 
  (->cmyk [this] this) 
  (->cmy  [this] (cpu/convert-cmyk this cc/cmyk->cmy make-cmy ))
  )

(defn make-hsl 
  ([h s l]   (make-hsl [h s l] 1))
  ([h s l a]
    (HSLa. h s l a)))

(defn make-rgb 
  ([r]       (make-rgb r r r 1))
  ([r g b]   (make-rgb r g b 1))
  ([r g b a] (RGBa. r g b a)))

(defn make-cmy 
  ([c]       (make-cmy c c c 1))
  ([c m y]   (make-cmy c m y 1))
  ([c m y a] (CMYa. c m y a)))

(defn make-cmyk 
  ([c k]       (make-cmyk c c c k 1))
  ([c m y k]   (make-cmyk c m y k 1))
  ([c m y k a] (CMYKa. c m y k a)))

(defn int->rgb [i]
  (let [r (bit-shift-right i 16)
        g (bit-and 0x0000ff (bit-shift-right i 8))
        b (bit-and 0x0000ff i)]
    (make-rgb r g b)))

(defn rgb [color]
  (um/select-vals (->rgb color) :red :green :blue))

(defn rgba [color]
  (um/select-vals (->rgb color) :red :green :blue :alpha))

(defn hsl [color]
  (um/select-vals (->hsl color) :hue :saturation :lightness))

(defn cmy [color]
  (um/select-vals (->cmy color) :cyan :magenta :yellow))

(defn cmyk [color]
  (um/select-vals (->cmyk color) :cyan :magenta :yellow :k))

(defn alpha
  ([color]           (:alpha color))
  ([color new-value] (assoc  color :alpha (umath/clamp 0 1 new-value))))

(def a alpha) ;; abbreviations

(defn red
  ([color]           (:red   (->rgb color)))
  ([color new-value] (assoc  (->rgb color) :red   (int (umath/clamp 0 255 new-value)))))

(def r red)

(defn green
  ([color]           (:green (->rgb color)))
  ([color new-value] (assoc  (->rgb color) :green (int (umath/clamp 0 255 new-value)))))

(def g green)

(defn blue
  ([color]           (:blue  (->rgb color)))
  ([color new-value] (assoc  (->rgb color) :blue  (int (umath/clamp 0 255 new-value)))))

(def b blue)

(defn hue
  ([color]           (:hue  (->hsl color)))
  ([color new-value] (assoc (->hsl color) :hue (mod new-value 360))))

(def h hue)

(defn saturation  
  ([color]           (:saturation (->hsl color)))
  ([color new-value] (assoc (->hsl color) :saturation (umath/clamp 0 1 new-value))))
  
(def s saturation)

(defn lightness
  ([color]           (:lightness (->hsl color)))
  ([color new-value] (assoc (->hsl color) :lightness (umath/clamp 0 1 new-value))))

(def l lightness)

(defn cyan
  ([color]           (:cyan (->cmy color)))
  ([color new-value] (assoc (->cmy color) :cyan (umath/clamp 0 1 new-value))))

(def c cyan)

(defn magenta  
  ([color]           (:magenta (->cmy color)))
  ([color new-value] (assoc (->cmy color) :megenta (umath/clamp 0 1 new-value))))
  
(def m magenta)

(defn yellow
  ([color]           (:yellow (->cmy color)))
  ([color new-value] (assoc (->cmy color) :yellow (umath/clamp 0 1 new-value))))

(def y yellow)

(defn k-cyan
  ([color]           (:cyan (->cmyk color)))
  ([color new-value] (assoc (->cmyk color) :cyan (umath/clamp 0 1 new-value))))

(def kc k-cyan)

(defn k-magenta  
  ([color]           (:magenta (->cmyk color)))
  ([color new-value] (assoc (->cmyk color) :megenta (umath/clamp 0 1 new-value))))
  
(def km k-magenta)

(defn k-yellow
  ([color]           (:yellow (->cmyk color)))
  ([color new-value] (assoc (->cmyk color) :yellow (umath/clamp 0 1 new-value))))

(def ky k-yellow)

(defn k
  ([color]           (:k color))
  ([color new-value] (assoc (->cmyk color) :k (umath/clamp 0 1 new-value))))
