(ns task2.t2-2)


(defn calc-col [a b h] (* h (/ (+ a b) 2)))


(defn integrals [f h] (map first (iterate (fn [[prev x f]] [(+ prev (calc-col (f x) (f (+ x h)) h)) (+ x h) f]) [0 0 f])))


(defn calc-integral [f x] (if (< x 0) (* -1 (calc-integral f (* -1 x)))
                                      (let [h 0.025
                                            k (int (/ x h))
                                            s (* k h)] (+ (calc-col (f s) (f x) (- x s)) (nth (integrals f h) k)))))


(defn integrate [f]
  (fn [x] (calc-integral f x)))


(defn sin [x] (Math/sin x))


(defn -main [& args]
  (time ((integrate sin) 10.14))
  (time ((integrate sin) 98))
  (time ((integrate sin) 100))
  (time ((integrate sin) 140))
  (time ((integrate sin) 98))
  (time ((integrate sin) 115))
  (time ((integrate sin) 115.34)))
