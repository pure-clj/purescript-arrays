(ns Data.Array.ST.Partial._foreign
  (:import java.util.ArrayList))

(defn peekImpl [i]
  (fn [xs]
    (fn [& _]
      (.get xs i))))

(defn pokeImpl [i]
  (fn [a]
    (fn [xs]
      (fn [& _]
        (.set xs i a)
        {}))))
