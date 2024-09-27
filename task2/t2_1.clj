(ns task2.t2-1)


(defn calc-col [a b h] (* h (/ (+ a b) 2)))


(def calc-on-grid (memoize (fn [f h k] (let [c (int (/ 10.0 h))
                                             l (max 0 (- k c (mod k c)))] (if (= k 0) 0
                                                                                      (+ (reduce (fn [i x] (+ i (calc-col (f (* (dec x) h)) (f (* x h)) h))) (range l (inc k)))
                                                                                         (calc-on-grid f h l)))))))


(def calc-integral (memoize (fn [f x] (if (< x 0) "incorrect argument"
                                                  (let [h 0.25
                                                        k (int (/ x h))
                                                        s (* k h)]
                                                    (+ (calc-col (f s) (f x) (- x s)) (calc-on-grid f h k)))))))


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