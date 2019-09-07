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
