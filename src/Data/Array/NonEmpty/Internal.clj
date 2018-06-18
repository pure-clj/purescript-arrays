(ns Data.Array.NonEmpty.Internal._foreign)

(defn fold1Impl [f]
  (fn [xs]
    (reduce f xs)))

(defn traverse1Impl []
  (let [Cont (fn [f]
               {:fn f, :type :Cont})
        cons-list (fn [x]
                    (fn [xs]
                      (apply list x xs)))]
    (fn [apply*]
      (fn [map*]
        (fn [f]
          (fn [array]
            (let [acc ((map* list) (f (last array)))
                  result (atom nil)])))))))
