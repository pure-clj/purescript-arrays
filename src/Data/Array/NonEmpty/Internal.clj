(ns Data.Array.NonEmpty.Internal._foreign)

(defn fold1Impl [f]
  (fn [xs]
    (reduce (fn [x1 x2]
              ((f x1) x2))
            xs)))

(defn- foldr [f acc coll]
  (if (first coll)
    (f (first coll) (foldr f acc (rest coll)))
    acc))

(defn traverse1Impl [apply*]
  (fn [map*]
    (fn [f]
      (fn [array]
        (let [cons* (fn [x]
                      (fn [xs]
                        (cons x xs)))
              acc ((map* (fn [x] (list x))) (f (last array)))
              build-from (fn [x ys]
                           ((apply* ((map* cons*) (f x))) ys))
              result-list (foldr build-from acc (vec (butlast array)))]
          ((map* vec) result-list))))))
