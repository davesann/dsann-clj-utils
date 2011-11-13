(ns utils.color.fns
  (:require 
    [utils.x.core :as u]
    [utils.map :as um]
    [utils.math :as umath]
    [clojure.string :as string]
    
    [utils.color.protocols :as cp]
  )
)

(defn adjust-color [color prop adjust-fn amount]
  "use to adjust color feature. Assumes that the appropriate color type 
   for the key is passed"
  (let [v (prop color)
        new-v (adjust-fn v amount)]
    (if (= v new-v)
      color
      (prop color new-v))))


(defn adjust-hsl [color hsl-prop adjust-fn amount]
  "use to adjust HSL features"
  (adjust-color (cp/->hsl color) hsl-prop adjust-fn amount))
    
(defn adjust-rgb [color rgb-prop adjust-fn amount]
  "use to adjust RGB features"
  (adjust-color (cp/->rgb color) rgb-prop adjust-fn amount))

(defn adjust-cmy [color hsl-prop adjust-fn amount]
  "use to adjust CMY features"
  (adjust-color (cp/->cmy color) hsl-prop adjust-fn amount))
    
(defn adjust-cmyk [color rgb-prop adjust-fn amount]
  "use to adjust CMYK features"
  (adjust-color (cp/->cmyk color) rgb-prop adjust-fn amount))


(defn fade        [color f amount] (adjust-color color cp/a f amount))

(defn saturate    [color f amount] (adjust-hsl color cp/s f amount))
(defn lighten     [color f amount] (adjust-hsl color cp/l f amount))
(defn rotate      [color f amount] (adjust-hsl color cp/h f amount))

(defn redify      [color f amount] (adjust-rgb color cp/r f amount))
(defn greenify    [color f amount] (adjust-rgb color cp/g f amount))
(defn blueify     [color f amount] (adjust-rgb color cp/b f amount))

(defn cyanify     [color f amount] (adjust-cmy color cp/c f amount))
(defn magentify   [color f amount] (adjust-cmy color cp/m f amount))
(defn yellify     [color f amount] (adjust-cmy color cp/y f amount))

(defn k-cyanify   [color f amount] (adjust-cmyk color cp/kc f amount))
(defn k-magentify [color f amount] (adjust-cmyk color cp/km f amount))
(defn k-yellify   [color f amount] (adjust-cmyk color cp/ky f amount))
(defn k-ify       [color f amount] (adjust-cmyk color cp/k  f amount))


(defn hsl-map [f colors]
  (let [hsl-colors (map cp/->hsl colors)
        h (apply f (map cp/h hsl-colors))
        s (apply f (map cp/s hsl-colors))
        l (apply f (map cp/l hsl-colors))]
    (cp/make-hsl h s l)))

(defn hsl-min [f & colors] (hsl-map min colors))
(defn hsl-max [f & colors] (hsl-map max colors))
(defn hsl-average [& colors]
  (let [hsl-colors (map cp/->hsl colors)
        hs (map cp/h hsl-colors)
        ss (map cp/s hsl-colors)
        ls (map cp/l hsl-colors)
        
        l (apply umath/average ls)
        
        [h s] (if-let [av (apply umath/average-angles-deg hs)]
                [av (apply umath/average ss)]
                [0 0])
        ]
    (cp/make-hsl h s l)))


(defn rgb-map [f colors]
  (let [rgb-colors (map cp/->rgb colors)
        r (apply f (map cp/r rgb-colors))
        g (apply f (map cp/g rgb-colors))
        b (apply f (map cp/b rgb-colors))]
    (cp/make-rgb r g b)))

(defn rgb-min [f & colors]     (rgb-map min colors))
(defn rgb-max [f & colors]     (rgb-map max colors))
(defn rgb-average [f & colors] (rgb-map umath/average colors))

(defn rgb-euclidian [c1 c2]
  (umath/euclidian (cp/rgb c1) (cp/rgb c2)))

(defn rgb-weighted-euclidian [c1 c2]
  (let [[rs gs bs] (map umath/square (map - (cp/rgb c1) (cp/rgb c2)))]
    (umath/sqrt (+ (* 3 rs) (* 4 gs) (* 2 bs)))))

;; comparison
(defn compare-colors [f color1 color2]
  (compare (f color1) (f color2)))

; RGB
(defn cmp-red [color1 color2] 
  (compare-colors cp/red color1 color2))

(defn cmp-blue [color1 color2] 
  (compare-colors cp/blue color1 color2))

(defn cmp-green [color1 color2] 
  (compare-colors cp/green color1 color2))


;HSL
(defn cmp-hue [color1 color2] 
  (compare-colors cp/hue color1 color2))

(defn cmp-saturation [color1 color2] 
  (compare-colors cp/saturation color1 color2))

(defn cmp-lightness [color1 color2] 
  (compare-colors cp/lightness color1 color2))


;CMY
(defn cmp-cyan [color1 color2] 
  (compare-colors cp/cyan color1 color2))

(defn cmp-magenta [color1 color2] 
  (compare-colors cp/magenta color1 color2))

(defn cmp-yellow [color1 color2] 
  (compare-colors cp/yellow color1 color2))


;CMYK
(defn cmp-k-cyan [color1 color2] 
  (compare-colors cp/k-cyan color1 color2))

(defn cmp-k-magenta [color1 color2] 
  (compare-colors cp/k-magenta color1 color2))

(defn cmp-k-yellow [color1 color2] 
  (compare-colors cp/k-yellow color1 color2))

(defn cmp-k [color1 color2] 
  (compare-colors cp/k color1 color2))


;; 

(defn color-more? [more? f color1 color2]
  (let [v1 (f color1)
        v2 (f color2)]
    (if (more? v1 v2)
      v1
      v2)))

(defn redder? [color1 color2]
  (color-more? > cp/red color1 color2))

(defn bluer? [color1 color2]
  (color-more? > cp/blue color1 color2))

(defn greener? [color1 color2]
  (color-more? > cp/green color1 color2))
  
(defn lighter? [color1 color2]
  (color-more? > cp/lightness color1 color2))

(defn more-saturated? [color1 color2]
  (color-more? > cp/saturation color1 color2))
