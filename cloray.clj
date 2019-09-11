(ns user
  (:import [java.awt Graphics2D Color Font]
           [java.awt.image BufferedImage]
           [javax.imageio ImageIO]
           [java.io File]
           [javax.swing JFrame JLabel JButton JPanel]
           [java.awt.event WindowListener]
           ))

(defn v
  ([x] {:x x})
  ([x y] {:x x :y y})
  ([x y z] {:x x :y y :z z}))
(defn map-kv [m f] (reduce-kv #(assoc %1 %2 (f %3)) {} m))
(defn scale [scalar vec] (map-kv vec #(* scalar %)))
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
(defn line-sphere-intersections [line sphere]
  (let [ctoo (vsub (:pos line) (:center sphere))
        disc (- (sq (vdot (:vec line) ctoo)) (- (sq (vlength ctoo)) (sq (:radius sphere))))
        ]
    (cond
      (< disc (- epsilon)) []
      (< (Math/abs disc) epsilon) [(walk-line (- (vdot (:vec line) ctoo)) line)]
      :else (let [f (- (vdot (:vec line) ctoo))
                  s (Math/sqrt disc)] (map #(walk-line % line) [(- f s) (+ f s)]))
      )
    )
  )
(defn angle [u v] (Math/acos (/ (vdot u v) (* (vlength u) (vlength v)))))
(comment
  "two, one and zero intersections"
  (line-sphere-intersections {:pos (v 0 0 0) :vec (v 1 0 0)} {:center (v 0 0 0) :radius 1.0})
  (line-sphere-intersections {:pos (v 0 1 0) :vec (v 1 0 0)} {:center (v 0 0 0) :radius 1.0})
  (line-sphere-intersections {:pos (v 0 2 0) :vec (v 1 0 0)} {:center (v 0 0 0) :radius 1.0})
  )

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
        (vadd (v -1 -1 0) (vadd (scale (/ x width) (v 2 0 0)) (scale (/ y height) (v 0 2 0))))
        ray {:pos view-plane-pos :vec (v 0 0 1)}
        gc (fn [sphere]
             (case (count (line-sphere-intersections ray sphere))
               0 nil
               1 Color/GREEN
               2 Color/BLUE
               ))
        col (first (filter #(gc %) (:spheres scene)))
        ]
    ;; (filter gc (:spheres scene))
    (if col (:color col))))
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
  (.add (.getContentPane frame) cv)
  (.revalidate frame)
  (mk-col image Color/BLUE)
  (mk-col image Color/RED)
  (.repaint cv)
  (.dispose frame)
  (.toFront frame)
  )

(mk-col image Color/BLACK)
(paint-func image calc-pixel)
(.repaint cv)
