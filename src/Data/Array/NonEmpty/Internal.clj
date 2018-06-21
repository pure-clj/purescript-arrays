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
          (letfn [(build-from [x ys]
                    ((apply* ((map* cons-list) (f x))) ys))
                  (go [acc curr-len xs]
                      (if (zero? curr-len)
                        acc
                        (let [last* (nth xs (dec curr-len))]
                          (Cont (fn []
                                  (go (build-from last* acc) (dec curr-len) xs))))))]
            (fn [array]
              (let [acc ((map* list) (f (last array)))
                    result (atom (go acc (count array) array))]
                (while (= :Cont (:type @result))
                  (reset! result ((:fn @result))))
                ((map* vec) @result)))))))))
