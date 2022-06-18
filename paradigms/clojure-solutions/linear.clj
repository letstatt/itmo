; basics

(defn size-equality? [sizes]
  (every? (partial == (first sizes)) sizes))

(defn apply-cond [cond f]
  (fn [& args]
    {:pre [(every? (partial cond (first args)) args)]}
    (apply mapv f args)))

(defn create-operation [cond]
  (partial apply-cond cond))

(defn is-scalars? [x]
  (every? number? x))

; vectors

(defn is-vector? [x]
  (and
    (vector? x)
    (every? number? x)))

(defn vector-sizes-equality? [& args]
  (and
    (every? is-vector? args)
    (size-equality? (mapv count args))))

(defn vector-operation [f]
  ((create-operation vector-sizes-equality?) f))

(def v+ (vector-operation +))
(def v- (vector-operation -))
(def v* (vector-operation *))
(def vd (vector-operation /))

(defn scalar [& args]
  {:pre [(apply vector-sizes-equality? args)]}
  (apply + (apply v* args)))

(defn vect
  ([& args]
   {:pre [(every? vector-sizes-equality? args) (every? (partial == 3) (mapv count args))]}
   (letfn [(vect-reduce [[a, b, c], [d, e, f]] (vector (- (* b f) (* c e)) (- (* c d) (* a f)) (- (* a e) (* b d))))]
     (reduce vect-reduce args))))

(defn v*s [v & args]
  {:pre [(is-vector? v) (is-scalars? args)]}
  (let [const (apply * args)]
    (mapv (partial * const) v)))

; matrices

(defn is-matrix? [x]
  (and
    (vector? x)
    (apply vector-sizes-equality? x)))

(defn matrix-sizes-equality? [& args]
  (and
    (every? is-matrix? args)
    (size-equality? (mapv count args))
    (size-equality? (mapv #(count (first %)) args))))

(defn matrix-operation [f]
  ((create-operation matrix-sizes-equality?) f))

(def m+ (matrix-operation v+))
(def m- (matrix-operation v-))
(def m* (matrix-operation v*))
(def md (matrix-operation vd))

(defn m*s [m & args]
  {:pre [(is-matrix? m) (is-scalars? args)]}
  (let [const (apply * args)]
    (mapv #(v*s % const) m)))

(defn m*v [m v]
  {:pre [(and
           (is-matrix? m)
           (is-vector? v)
           (== (count (first m)) (count v)))]}
  (mapv (partial scalar v) m))

(defn transpose [m]
  {:pre [(is-matrix? m)]}
  (apply mapv vector m))

(defn m*m
  ([& args] {:pre [(every? is-matrix? args)]}
   (letfn [(mmul-reduce [a b]
             {:pre [(== (count (first a)) (count b))]}
             (mapv #(m*v (transpose b) %) a))]
     (reduce mmul-reduce args))))

; simplexes

(defn is-simplex? [x]
  {:pre [(> (count x) 0)]}
  (or
    (is-vector? x)
    (and
      (== (count (reduce #(if (== (count %1) (inc (count %2))) %2 []) (conj x 1) x)) (count (last x)))
      (every? is-simplex? x))))

(defn simplex-sizes-equality? [& args]
  (and
    (every? is-simplex? args)
    (size-equality? (mapv count args))))

(defn simplex-operation [f]
  (letfn [(operate [& args] (if (is-vector? (first args)) (apply f args) (apply mapv operate args)))]
    (fn
      ([arg] {:pre [(is-simplex? arg)]}
       (operate arg))
      ([arg & args]
       {:pre [(apply simplex-sizes-equality? arg args)]}
       (reduce operate arg args)))))

(def x+ (simplex-operation v+))
(def x- (simplex-operation v-))
(def x* (simplex-operation v*))
(def xd (simplex-operation vd))