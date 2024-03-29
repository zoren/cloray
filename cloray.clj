(ns user
  (:import
   [java.awt Color]
   [java.awt.image BufferedImage]
   [javax.imageio ImageIO]
   [java.io File]
   ))

(defn scale [scalar vec] (mapv #(* scalar %) vec))
(defn vadd [a & more] (reduce #(mapv + %1 %2) a more))
(defn vsub [a b] (mapv - a b))
(defn vdot [a b] (apply + (map * a b)))
(defn vlength [v] (Math/sqrt (reduce (fn [acc d] (+ acc (* d d))) 0.0 v)))
(defn norm [v] (scale (/ 1 (vlength v)) v))
(defn walk-line [scalar line] (vadd (:pos line) (scale scalar (:vec line))))
(defn sq [x] (* x x))
(def epsilon 1e-5)
(defn line-sphere-intersections-scale
  "returns the distance along the line to the sphere intersection closest to the line origin, if there is one"
  [line sphere]
  (let [center-to-line-origin (vsub (:pos line) (:center sphere))
        line-dot-center-to-line-origin (vdot (:vec line) center-to-line-origin)
        disc (- (sq line-dot-center-to-line-origin) (- (vdot center-to-line-origin center-to-line-origin) (:squared-radius sphere)))]
    (cond
      (< disc (- epsilon)) nil
      (< (Math/abs disc) epsilon) (- line-dot-center-to-line-origin)
      :else (- (- line-dot-center-to-line-origin) (Math/sqrt disc)))))

(def clamp #(max 0 (min 255 %)))
(defn render [scene]
  (let [squared-spheres (map #(assoc % :squared-radius (* (:radius %) (:radius %))) (:spheres scene))]
    (fn [{:keys [width height set-pixel]}]
      (dotimes [y height]
        (let [camera (:camera scene)
              yvec-pos (vadd (:pos camera) (scale (/ y height) (:yvec camera)))]
          (dotimes [x width]
            (let [view-plane-point (vadd yvec-pos (scale (/ x width) (:xvec camera)))
                  eye (:eye camera)
                  ray {:pos eye :vec (norm (vsub view-plane-point eye))}
                  sphere-dists (map (fn [sphere] {:scalar (line-sphere-intersections-scale ray sphere)
                                                  :sphere sphere}) squared-spheres)
                  ;; TODO we should remove intersections that are behind the camera here
                  sorted-sphere-dists (sort-by :scalar (filter :scalar sphere-dists))
                  closest-sphere-intersection (first sorted-sphere-dists)]
              (if closest-sphere-intersection
                (let
                    [closest-sphere (:sphere closest-sphere-intersection)
                     intersection (walk-line (:scalar closest-sphere-intersection) ray)
                     normal (norm (vsub intersection (:center closest-sphere)))
                     dot (vdot (norm (vsub intersection (:light scene))) normal)
                     g (fn [color-component] (clamp (* dot color-component)))]
                  (set-pixel x y (mapv g (:color closest-sphere))))))))))))

;; Java specific stuff starts here
;; https://stackoverflow.com/a/33749052
(defn mk-buf-img [width height] (BufferedImage. width height BufferedImage/TYPE_INT_ARGB))
(defn rgb-to-java-color [[r g b]] (Color. (int r) (int g) (int b)))
;; https://www.programcreek.com/java-api-examples/?class=java.awt.image.BufferedImage&method=setRGB
(defn set-pixel-image [image x y color]
  (.setRGB image (int x) (int y) (.getRGB (rgb-to-java-color color))))
(defn paint-func [image f]
  (dotimes [y (. image getHeight)]
    (dotimes [x (. image getWidth)]
      (if-let [color (f x y (. image getWidth) (. image getHeight))]
        (set-pixel-image image x y color))) image))
(defn mk-col [image color] (paint-func image (constantly color)))

(defn write-png-file [image file-name] (ImageIO/write image "png" (File. (str "./" file-name ".png"))))

(defn render-image [image scene]
  ((render scene) {:width (. image getWidth)
                   :height (. image getHeight)
                   :set-pixel
                   (fn [x y color] (set-pixel-image image x y color))}))

(comment
  (def image (mk-buf-img 100 100))

  (def scene-three-balls
    {:spheres
     #{{:center [0.1 0.0 0.0]
        :radius 1
        :color [0 0 255]}
       {:center [-0.1 0.0 0.0]
        :radius 1
        :color [255 0 0]}
       {:center [0.0 0.0 0.0]
        :radius 1
        :color [0 255 0]}},
     :camera
     {:pos [-1 -1 0.5]
      :xvec [2 0 0]
      :yvec [0 2 0]
      :eye [0 0 1.5]}
     :light [100 100 -100]})

  ((render scene-three-balls) {:width 3 :height 3 :set-pixel println})

  (do
    (mk-col image [0 0 0])
    (time (render-image image scene-three-balls))
    image
    )
  )
