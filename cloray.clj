(ns user
  (:import
   [java.awt Color]
   [java.awt.image BufferedImage]
   [javax.imageio ImageIO]
   [java.io File]
   [javax.swing JFrame]
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
  (let [ctoo (vsub (:pos line) (:center sphere))
        line-dot-ctoo (vdot (:vec line) ctoo)
        disc (- (sq line-dot-ctoo) (- (vdot ctoo ctoo) (sq (:radius sphere))))]
    (cond
      (< disc (- epsilon)) nil
      (< (Math/abs disc) epsilon) (- line-dot-ctoo)
      :else (let [f (- line-dot-ctoo)
                  s (Math/sqrt disc)] (- f s)))))

(def v vector)
(def rgb vector)
(def spheres
  #{
    {:center (v -0.1 0.0 0.0)
     :radius 1
     :color (rgb 255 0 0)}
    {:center (v 0.0 0.0 0.0)
     :radius 1
     :color (rgb 0 255 0)}
    {:center (v 0.1 0.0 0.0)
     :radius 1
     :color (rgb 0 0 255)}
    })

(def clamp #(max 0 (min 255 %)))
;; https://stackoverflow.com/a/33749052
;; https://www.programcreek.com/java-api-examples/?class=java.awt.image.BufferedImage&method=setRGB
(defn calc-pixel [x y width height]
  (let [view-plane-pos
        (vadd (v -1 -1 10) (scale (/ x width) (v 2 0 0)) (scale (/ y height) (v 0 2 0)))
        ray {:pos view-plane-pos :vec (v 0 0 -1)}
        light (v 100 100 -100)
        sphere-dists (map (fn [sphere] [(line-sphere-intersections-scale ray sphere) sphere]) spheres)
        ;; TODO we should remove intersections that are behind the camera here
        sorted-sphere-dists (sort-by first (filter first sphere-dists))
        closest-sphere-pair (first (filter first sorted-sphere-dists))
        ]
    (if closest-sphere-pair
      (let
          [closest-sphere (second closest-sphere-pair)
           intersection (walk-line (first closest-sphere-pair) ray)
           normal (norm (vsub intersection (:center closest-sphere)))
           dot (vdot (norm (vsub intersection light)) normal)
           g (fn [color-component] (clamp (* dot color-component)))
           ]
        (mapv g (:color closest-sphere))
        )
      )
    ))

;; Java specific stuff starts here
(defn mk-buf-img [width height] (BufferedImage. width height BufferedImage/TYPE_INT_ARGB))
(defn rgb-to-java-color [[r g b]] (Color. (int r) (int g) (int b)))
(defn paint-func [image f]
  (dotimes [y (. image getHeight)]
    (dotimes [x (. image getWidth)]
      (if-let [color (f x y (. image getWidth) (. image getHeight))]
        (.setRGB image (int x) (int y) (.getRGB (rgb-to-java-color color))))) image))
(defn mk-col [image color] (paint-func image (constantly color)))

(defn write-png-file [image file-name] (ImageIO/write image "png" (File. (str "./" file-name ".png"))))

;; http://www.thebusby.com/2010/02/capturing-screenshot-displaying-image.html?m=1
(defn display-image
  "Displays an image in a new window"
  [image]
  (let [frame (doto (javax.swing.JFrame. "Display Image")
                (.setSize (+ 10 (. image getWidth)) (+ 30 (. image getHeight)))
                (.setVisible true))
        cv (proxy [java.awt.Canvas] [] (paint [g] (. g drawImage image nil nil)))]
    (.add (.getContentPane frame) cv)
    (.revalidate frame)))

(comment
  (def image (mk-buf-img 100 100))
  (def frame (doto (javax.swing.JFrame. "Display Image")
               (.setSize (+ 10 (. image getWidth)) (+ 30 (. image getHeight)))
               (.setVisible true)))
  (def cv (proxy [java.awt.Canvas] [] (paint [g] (. g drawImage image nil nil))))

  (do
    (.add (.getContentPane frame) cv)
    (.revalidate frame))
  (.dispose frame)
  (.toFront frame)

  (do
    (mk-col image (rgb 0 0 0))
    (time (paint-func image calc-pixel))
    (.repaint cv)
    )
  )
