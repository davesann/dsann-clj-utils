(ns utils.color.conversion
  (:require 
    [utils.x.core :as u]
    [utils.map :as um]
    [utils.math :as umath]
  )
)

;; Base color conversion functions

;; http://www.easyrgb.com/
(defn rgb->cmy [rgb-vals]
  (map #(umath/clamp 0 1 (- 1 (/ % 255))) 
       rgb-vals))

(defn cmy->rgb [cmy-vals]
  (map #(umath/round 
          (umath/clamp 0 255 (* 255 (- 1 %))))
       cmy-vals))

(defn cmy->cmyk [cmy-vals]
  (let [k (apply min cmy-vals)]
    (if (== k 1)
      [0 0 0 1]
      (let [mk (- 1 k)
            [ck mk yk] (map #(umath/clamp 0 1
                                          (/ (- % k) mk)) 
                            cmy-vals)]
        [ck mk yk k]))))

(defn cmyk->cmy [[c m y k]]
  (let [k1 (- 1 k)]
    (map #(umath/clamp 0 1 
                       (+ (* % k1) k)) 
         [c m y])))


;; Algorithm http://130.113.54.154/~monger/hsl-rgb.html (thank you)
(defn rgb->hsl [[r g b]]
  (let [rgb (um/mapvals 
              #( / % 255.0)
              {:red r :green g :blue b})
        [max-rgb-key max-rgb-value] (um/select-best > rgb)
        [min-rgb-key min-rgb-value] (um/select-best < rgb)
        sum  (+ max-rgb-value min-rgb-value)
        diff (- max-rgb-value min-rgb-value)
        l   (/ sum 2)
        [h s] (if (== max-rgb-value min-rgb-value)
                [0 0]
                (let [s (if (< l 0.5)
                          (/ diff sum)
                          (/ diff (- 2 sum)))
                          
                      h (let [{:keys [red green blue]} rgb]
                          (case max-rgb-key
                            :red (/ (- green blue) diff)
                            :green (+ 2 (/ (- blue red) diff))
                            :blue  (+ 4 (/ (- red green) diff))))
                      ]
                  [(* 60 h) s]))
        ]
    [(mod h 360) (umath/clamp 0 1 s) (umath/clamp 0 1 l)]))

(defn- hue->rgb [t1 t2 vH]
  (cond
    (< (* 6 vH) 1)     (+ t1 (* (- t2 t1) 6 vH))
    (< (* 2 vH) 1)     t2
    (< (* 3 vH) 2)     (+ t1 (* (- t2 t1) (- 2/3 vH) 6))
    true               t1))

(defn hsl->rgb [[h s l]]
  (map 
    #(umath/round (umath/clamp 0 255 (* % 255.0))) 
    (if (zero? s)
      [l l l]
      (let [t2 (if (< l 0.5)
                 (* l (inc s))
                 (- (+ l s) (* l s)))
            t1 (- (* 2 l) t2)
            h1 (/ h 360)]
        
        (map #(hue->rgb t1 t2 %)
             (map #(mod % 1)
                  [(+ h1 1/3)
                   h1
                   (- h1 1/3)]))))))

(defn rgb->hsv [[r g b]]
  (let [rgb (um/mapvals 
              #( / % 255.0)
              {:red r :green g :blue b})
        [max-rgb-key max-rgb-value] (um/select-best > rgb)
        [min-rgb-key min-rgb-value] (um/select-best < rgb)
        sum  (+ max-rgb-value min-rgb-value)
        diff (- max-rgb-value min-rgb-value)
        v max-rgb-value
        ]
    (if (zero? (u/log diff))
      [0 0 v]
      (let [s (/ diff max-rgb-value)
            [dr dg db] (map #(/ (+ (/ (- max-rgb-value %) 6)
                                  (/ diff 2))
                               diff)
                            (vals rgb))
            h (mod
                (case max-rgb-key
                  :red   (- db dg)
                  :green (- (+ 1/3 dr) db)
                  :blue  (- (+ 2/3 dg) dr))
                1)
            ]
        [h s v]))))

(defn hsv->rgb [[h s v]]
  (if (zero? s)
    (map #(* 255 %) v)
    (let [h6  (* 6 h)
          ih6 (int h6)
          v1 (* v (- 1 s))
          v2 (* v (- 1 (* s (- h6 ih6))))
          v3 (* v (- 1 (* s (- 1 (- h6 ih6)))))
          rgb (map #(umath/round (umath/clamp 0 255 (* 255 %)))
                   (case ih6
                     0 [v  v3 v1]
                     1 [v2 v  v1]
                     2 [v1 v  v3]
                     3 [v1 v2 v ]
                     4 [v3 v1 v ]
                     [v v1 v2]))
          ]
      rgb)))
                
;; Transitive closure
;; would be nice to derive these...
(defn rgb->cmyk [rgb-vals]
  (cmy->cmyk (rgb->cmy rgb-vals)))

(defn cmyk->rgb [cmky-vals]
  (cmy->rgb (cmyk->cmy cmky-vals)))

(defn cmy->hsl [cmy-vals]
  (rgb->hsl (cmy->rgb  cmy-vals)))

(defn hsl->cmy [hsl-vals]
  (rgb->cmy (hsl->rgb  hsl-vals)))

(defn cmyk->hsl [cmyk-vals]
  (rgb->hsl (cmyk->rgb  cmyk-vals)))

(defn hsl->cmyk [hsl-vals]
  (rgb->cmyk (hsl->rgb  hsl-vals)))



(defn check-conversion []
  (let [result (remove nil? (for [r (range 0 255)
                                  g (range 0 255)
                                  b (range 0 255)]
                              (let [[h s l] (rgb->hsl [r g b])
                                    [r1 g1 b1] (hsl->rgb [h s l])]
                                (if (not (and (= r r1) (= b b1) (= g g1)))
                                  (u/log [:error 
                                          {:r r :b b :g g}
                                          {:h h :s s :l l}
                                          {:r1 r1 :b1 b1 :g1 g1}])))))
        ]
    result))


