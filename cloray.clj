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
(defn map-vals [f m] (into {} (map (fn [[k v]] [k (f v)]) m)))
(defn scale [scalar vec] (map-vals #(* scalar %) vec))
(defn vadd [a b] (merge-with + a b))
(defn vsub [a b] (merge-with - a b))
(defn vdot [a b] (apply + (vals (merge-with * a b))))
(defn vlength [v] (Math/sqrt (vdot v v)))
(defn norm [v] (scale (/ 1 (vlength v)) v))
(defn dist [line point]
  (let [ptoa (vsub (:pos line) point)
        n (norm (:vec line))]
    (vlength (vsub ptoa (scale (vdot ptoa n) n)))))
(defn walk-line [scalar line] (vadd (:pos line) (scale scalar (:vec line))))
(defn sq [x] (* x x))
(def epsilon 1e-5)
(defn line-sphere-intersections-scales [line sphere]
  (let [ctoo (vsub (:pos line) (:center sphere))
        disc (- (sq (vdot (:vec line) ctoo)) (- (vdot ctoo ctoo) (sq (:radius sphere))))
        ]
    (cond
      (< disc (- epsilon)) []
      (< (Math/abs disc) epsilon) [(- (vdot (:vec line) ctoo))]
      :else (let [f (- (vdot (:vec line) ctoo))
                  s (Math/sqrt disc)] [(- f s) (+ f s)])
      )
    )
  )
(defn line-sphere-intersections [line sphere]
  (map #(walk-line % line) (line-sphere-intersections-scales line sphere)))

(def scene {:cam {:eye-pos (v 0.0 0.0 -1.0)
                  :view-plane
                  {:pos (v -1.0 -1.0 0.0)
                   :x (v 2.0 0.0 0.0)
                   :y (v 0.0 2.0 0.0)}
                  }
            :lights #{
                      {:pos (v 0.0 0.0 0.0)}
                      }
            :spheres #{
                       {:center (v 0.0 0.0 0.0)
                        :radius 1
                        :color Color/RED}
                       {:center (v 0.1 0.0 0.0)
                        :radius 1
                        :color Color/GREEN}
                       {:center (v -0.1 0.0 0.0)
                        :radius 1
                        :color Color/BLUE}
                       }})

;; https://stackoverflow.com/a/33749052
;; https://www.programcreek.com/java-api-examples/?class=java.awt.image.BufferedImage&method=setRGB
(defn calc-pixel [x y width height]
  (let [view-plane-pos
        (vadd (v -1 -1 10) (vadd (scale (/ x width) (v 2 0 0)) (scale (/ y height) (v 0 2 0))))
        ray {:pos view-plane-pos :vec (v 0 0 -1)}
        light (v 100 100 -100)
        sphere-dists (map (fn [sphere] [(first (line-sphere-intersections-scales ray sphere)) sphere]) (:spheres scene))
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
           g (fn [color-component] (int (max 0 (min 255 (* dot color-component)))))
           ]
        (Color. (g (.getRed color)) (g (.getGreen color)) (g (.getBlue color)))
        )
      )
    ))

(defn mk-buf-img [width height] (BufferedImage. width height BufferedImage/TYPE_INT_ARGB))
(defn paint-func [image f]
  (dotimes [y (. image getHeight)]
    (dotimes [x (. image getWidth)]
      (if-let [color (f x y (. image getWidth) (. image getHeight))]
        (.setRGB image (int x) (int y) (.getRGB color)))) image))
(defn mk-col [image color] (paint-func image (constantly color)))

(defn write-png-file [image file-name] (ImageIO/write image "png" (File. (str "./" file-name ".png"))))
(defn mk-argb-img-func [width height f]
  (let [image (mk-buf-img width height)]
    (dotimes [n height] (dotimes [m width] (.setRGB image (int m) (int n) (.getRGB (f m n)))))
    image))
(defn show-img [width height color] (mk-argb-img-func width height (fn [_x _y] color)))
(defn show-img-2 [width height color color2]
  (mk-argb-img-func width height
                    (fn [x y] (if (< x y) color color2))))
(defn calc-pixel [x y width height]
  (if (< (vlength (vsub (v x y) (v (/ width 2) (/ height 2)))) 50.0)
    (.getRGB Color/GREEN)
    ;; (if (< (vlength (vsub (v x y) (v 0 0))) 100.0)
    ;;   (.getRGB Color/RED))
    ))

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
    (mk-col image Color/BLACK)
    (paint-func image calc-pixel)
    (.repaint cv))
  )
