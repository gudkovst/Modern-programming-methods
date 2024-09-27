(defn my-map [f coll]
      (reduce (fn [coll x] (concat coll (list (f x)))) (list) coll))


(my-map (fn [x] (* x x)) (list 1 2 3 4 5))


(defn my-filter [pred coll]
      (reduce (fn [coll x] (if (pred x)
                               (concat coll (list x))
                               coll)) (list) coll))


(my-filter (fn [n] (= 0 (mod n 3))) (range 1 21))
