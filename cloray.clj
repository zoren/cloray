(ns user
  (:import [java.awt Color]
           [java.awt.image BufferedImage]
           [javax.imageio ImageIO]
           [java.io File]
           [javax.swing JFrame]
           ))

(defn v
  ([x] {:x x})
  ([x y] {:x x :y y})
  ([x y z] {:x x :y y :z z}))
(defn rgb [r g b] {:r r :g g :b b})
(defn map-vals [f m] (into {} (map (fn [[k v]] [k (f v)]) m)))
(defn scale [scalar vec] (map-vals #(* scalar %) vec))
(defn vadd [a b] (merge-with + a b))
(defn vsub [a b] (merge-with - a b))
(defn vdot [a b] (apply + (vals (merge-with * a b))))
(defn vlength [v] (Math/sqrt (vdot v v)))
(defn norm [v] (scale (/ 1 (vlength v)) v))
(defn walk-line [scalar line] (vadd (:pos line) (scale scalar (:vec line))))
(defn sq [x] (* x x))
(def epsilon 1e-5)
(defn line-sphere-intersections-scale [line sphere]
  "returns the distance along the line to the sphere intersection closest to the line origin, if there is one"
  (let [ctoo (vsub (:pos line) (:center sphere))
        disc (- (sq (vdot (:vec line) ctoo)) (- (vdot ctoo ctoo) (sq (:radius sphere))))
        ]
    (cond
      (< disc (- epsilon)) nil
      (< (Math/abs disc) epsilon) (- (vdot (:vec line) ctoo))
      :else (let [f (- (vdot (:vec line) ctoo))
                  s (Math/sqrt disc)] (- f s))
      )
    )
  )

(def spheres
  #{
    {:center (v 0.0 0.0 0.0)
     :radius 1
     :color (rgb 255 0 0)}
    {:center (v 0.1 0.0 0.0)
     :radius 1
     :color (rgb 0 255 0)}
    {:center (v -0.1 0.0 0.0)
     :radius 1
     :color (rgb 0 0 255)}
    })

;; https://stackoverflow.com/a/33749052
;; https://www.programcreek.com/java-api-examples/?class=java.awt.image.BufferedImage&method=setRGB
(defn calc-pixel [x y width height]
  (let [view-plane-pos
        (vadd (v -1 -1 10) (vadd (scale (/ x width) (v 2 0 0)) (scale (/ y height) (v 0 2 0))))
        ray {:pos view-plane-pos :vec (v 0 0 -1)}
        light (v 100 100 -100)
        sphere-dists (map (fn [sphere] [(line-sphere-intersections-scale ray sphere) sphere]) spheres)
        ;; TODO we should remove intersections that are behind the camera here
        sorted-sphere-dists (sort-by first (filter first sphere-dists))
        closest-sphere-pair (first (filter first sorted-sphere-dists))
        closest-sphere (second closest-sphere-pair)
        ]
    (if closest-sphere
      (let
          [intersection (if closest-sphere-pair (walk-line (first closest-sphere-pair) ray))
           normal (norm (vsub intersection (:center closest-sphere)))
           dot (vdot (norm (vsub intersection light)) normal)
           color (:color closest-sphere)
           clamp #(max 0 (min 255 %))
           g (fn [color-component] (clamp (* dot color-component)))
           ]
        (map-vals g color)
        )
      )
    ))

;; Java specific stuff starts here
(defn mk-buf-img [width height] (BufferedImage. width height BufferedImage/TYPE_INT_ARGB))
(defn rgb-to-java-color [{:keys [r g b]}] (Color. (int r) (int g) (int b)))
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
