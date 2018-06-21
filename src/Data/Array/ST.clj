(ns Data.Array.ST._foreign
  (:refer-clojure :exclude [empty])
  (:import [java.util ArrayList Collection Collections Comparator]))

(defn empty []
  (ArrayList.))

(defn peekImpl [just]
  (fn [nothing]
    (fn [i]
      (fn [^ArrayList xs]
        (fn []
          (if (> (.size xs) i -1)
            (just (.get xs i))
            nothing))))))

(defn poke [i]
  (fn [a]
    (fn [^ArrayList xs]
      (fn []
        (let [in-bounds (> (.size xs) i -1)]
          (when in-bounds
            (.set xs i a))
          in-bounds)))))

(defn pushAll [as]
  (fn [^ArrayList xs]
    (fn []
      (.addAll xs as)
      (.size xs))))

(defn splice [i]
  (fn [how-many]
    (fn [bs]
      (fn [^ArrayList xs]
        (fn []
          (let [^long start (if (< i 0)
                              (+ i (.size xs))
                              i)]
            (when (pos? how-many)
              (dotimes [_ how-many]
                (let [^int idx start]
                  (.remove xs idx))))
            (when (pos? (count bs))
              (.addAll xs start bs))))))))

(defn copyImpl [^Collection xs]
  (fn []
    (ArrayList. xs)))

(defn sortByImpl [comp]
  (fn [^ArrayList xs]
    (fn []
      (Collections/sort xs
                        (reify Comparator
                          (compare [_ x1 x2]
                            ((comp x1) x2)))))))

(defn toAssocArray [^ArrayList xs]
  (fn []
    (vec (map-indexed (fn [i v]
                        {"index" i
                         "value" v})
                      xs))))
