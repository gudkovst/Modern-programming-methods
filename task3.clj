(ns task3.task3)

(defn my-partition [size lst] (map (fn [n] (take size (drop n lst))) (range 0 (count lst) size)))


(def block-size 25)


(defn take-not-empty [[x & xs]] (if (empty? x) (lazy-seq) (lazy-seq (cons x (take-not-empty xs)))))


(defn merge [[x & xs]] (if (empty? xs) (lazy-seq) (lazy-cat (deref x) (merge xs))))


(defn p-filter-1 [pred coll] (->> (my-partition block-size coll)
                                  (map #(future (doall (filter pred %))))
                                  (doall)
                                  (merge)))


(defn p-filter-2 [pred coll] (->> (iterate #(drop block-size %) coll)
                                  (map #(take block-size %))
                                  (take-not-empty)
                                  (map #(future (doall (filter pred %))))
                                  (merge)))


(def heavy-pred (fn [n] (Thread/sleep 1) (= 0 (mod n 10))))


(defn -main [& args]
  (time (->> (range 500)
             (filter heavy-pred)
             (doall)))

  (time (->> (range 500)
             (p-filter-1 heavy-pred)
             (doall)))

  (time (->> (range 500)
             (p-filter-2 heavy-pred)
             (doall)))

  (time (->> (iterate inc 0)
             (filter heavy-pred)
             (take 50)
             (doall)))

  (time (->> (iterate inc 0)
             (p-filter-2 heavy-pred)
             (take 50)
             (doall)))

  ;(time (println (filter heavy-pred (range 500))))
  ;(time (println (take 20 (p-filter-1 heavy-pred (range 500)))))
  ;(time (println (take 20 (p-filter-2 heavy-pred (iterate inc 0)))))
  )
