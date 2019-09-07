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
(comment
  "two, one and zero intersections"
  (line-sphere-intersections {:pos (v 0 0 0) :vec (v 1 0 0)} {:center (v 0 0 0) :radius 1.0})
  (line-sphere-intersections {:pos (v 0 1 0) :vec (v 1 0 0)} {:center (v 0 0 0) :radius 1.0})
  (line-sphere-intersections {:pos (v 0 2 0) :vec (v 1 0 0)} {:center (v 0 0 0) :radius 1.0})
  )
