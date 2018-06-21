(ns Data.Array.ST.Partial._foreign
  (:import java.util.ArrayList))

(defn peekImpl [i]
  (fn [xs]
    (fn []
      (.get xs i))))

(defn pokeImpl [i]
  (fn [a]
    (fn [xs]
      (fn []
        (.set xs i a)))))
