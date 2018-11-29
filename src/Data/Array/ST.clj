(ns Data.Array.ST._foreign
  (:refer-clojure :exclude [empty])
  (:import [java.util ArrayList Collection Collections Comparator]))

(defn empty [& _]
  (ArrayList.))

(defn peekImpl [just]
  (fn [nothing]
    (fn [i]
      (fn [^ArrayList xs]
        (fn [& _]
          (if (> (.size xs) i -1)
            (just (.get xs i))
            nothing))))))

(defn poke [i]
  (fn [a]
    (fn [^ArrayList xs]
      (fn [& _]
        (let [in-bounds (> (.size xs) i -1)]
          (when in-bounds
            (.set xs i a))
          in-bounds)))))

(defn pushAll [as]
  (fn [^ArrayList xs]
    (fn [& _]
      (.addAll xs as)
      (.size xs))))

(defn splice [i]
  (fn [how-many]
    (fn [bs]
      (fn [^ArrayList xs]
        (fn [& _]
          (let [^long start (if (< i 0)
                              (+ i (.size xs))
                              i)
                removed (if (pos? how-many)
                          (mapv (fn [& _]
                                  (let [^int idx start]
                                    (.remove xs idx)))
                                (range 0 how-many))
                          [])]
            (when (pos? (count bs))
              (.addAll xs start bs))
            removed))))))

(defn copyImpl [^Collection xs]
  (fn [& _]
    (ArrayList. xs)))

(defn copyBackImpl [^ArrayList as]
  (fn [& _]
    (vec as)))

(defn sortByImpl [comp]
  (fn [^ArrayList xs]
    (fn [& _]
      (Collections/sort xs
                        (reify Comparator
                          (compare [_ x1 x2]
                            ((comp x1) x2))))
      xs)))

(defn toAssocArray [^ArrayList xs]
  (fn [& _]
    (vec (map-indexed (fn [i v]
                        {"index" i
                         "value" v})
                      xs))))
