(ns Data.Array._foreign
  (:refer-clojure :exclude [range
                            replicate
                            cons
                            reverse
                            concat
                            filter
                            partition
                            take
                            drop])
  (:require [clojure.core :as c])
  (:import java.util.Comparator))

(defn range [start]
  (fn [end]
    (let [inv? (> start end)
          [s e] (if inv?
                  [end start]
                  [start end])
          res (c/range s (inc e))]
      (if inv?
        (vec (c/reverse res))
        (vec res)))))

(defn replicate [count*]
  (fn [value]
    (vec (repeat count* value))))

(defn fromFoldableImpl [foldr]
  (fn [xs]
    (let [curry-cons (fn [head]
                       (fn [tail]
                         (apply list head tail)))]
      (vec (((foldr curry-cons) ()) xs)))))

(defn length [xs]
  (count xs))

(defn cons [e]
  (fn [l]
    (into [e] l)))

(defn snoc [l]
  (fn [e]
    (conj l e)))

(defn uncons' [empty]
  (fn [next*]
    (fn [xs]
      (if (empty? xs)
        (empty {})
        ((next* (first xs)) (vec (next xs)))))))

(defn indexImpl [just]
  (fn [nothing]
    (fn [xs]
      (fn [i]
        (if (> (count xs) i -1)
          (just (nth xs i))
          nothing)))))

(defn findIndexImpl [just]
  (fn [nothing]
    (fn [f]
      (fn [xs]
        (if-let [res (reduce
                      (fn [_ i]
                        (when (f (nth xs i))
                          (reduced i)))
                      nil
                      (clojure.core/range 0 (count xs)))]
          (just res)
          nothing)))))

(defn findLastIndexImpl [just]
  (fn [nothing]
    (fn [f]
      (fn [xs]
        (if-let [res (reduce
                      (fn [_ i]
                        (when (f (nth xs i))
                          (reduced i)))
                      nil
                      (c/reverse (c/range 0 (count xs))))]
          (just res)
          nothing)))))

(defn _insertAt [just]
  (fn [nothing]
    (fn [i]
      (fn [a]
        (fn [xs]
          (if (> (count xs) i -1)
            (as-> xs x
                 (split-at i x)
                 (c/concat (first x) [a] (second x))
                 (vec x)
                 (just x))
            nothing))))))

(defn _deleteAt [just]
  (fn [nothing]
    (fn [i]
      (fn [xs]
        (if (> (count xs) i -1)
          (as-> xs x
            (split-at i x)
            (c/concat (first x) (next (second x)))
            (vec x)
            (just x))
          nothing)))))

(defn _updateAt [just]
  (fn [nothing]
    (fn [i]
      (fn [a]
        (fn [xs]
          (if (> (count xs) i -1)
            (->> xs
              (map-indexed (fn [idx x]
                             (if (= i idx)
                               a
                               x)))
              (vec)
              (just))
            nothing))))))

(defn reverse [xs]
  (vec (clojure.core/reverse xs)))

(defn concat [xss]
  (vec (apply c/concat xss)))

(defn filter [f]
  (fn [xs]
    (filterv f xs)))

(defn partition [f]
  (fn [xs]
    {"yes" (filterv f xs)
     "no" (filterv (complement f) xs)}))

(defn sortImpl [f]
  (fn [xs]
    (vec (sort (reify Comparator
                 (compare [_ x1 x2]
                   ((f x1) x2)))
               xs))))

(defn slice [s]
  (fn [e]
    (fn [xs]
      (let [start (if (neg? s)
                    (+ (count xs) s)
                    s)
            end (if (neg? e)
                  (+ (count xs) e)
                  e)]
        (reduce (fn [acc [i v]]
                  (if (<= start i (dec end))
                    (conj acc v)
                    acc))
                []
                (map vector (c/range (count xs)) xs))))))

(defn take [n]
  (fn [xs]
    (vec (c/take n xs))))

(defn drop [n]
  (fn [xs]
    (vec (c/drop n xs))))

(defn zipWith [f]
  (fn [xs]
    (fn [ys]
      (mapv (fn [x y]
              ((f x) y))
            xs
            ys))))

(defn unsafeIndexImpl [xs]
  (fn [n]
    (nth xs n)))
