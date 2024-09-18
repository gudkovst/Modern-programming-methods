(defn merge [lst]
      (reduce concat lst))


(defn filter-alf [alfabet word]
      (filter (fn [y] (not (.endsWith (str word) y))) alfabet))


(defn extension [cur-list alfabet]
      (merge (map (fn [x] (map (fn [y] (.concat (str x) y)) (filter-alf alfabet x))) cur-list)))


(defn construct-words-function [alfabet n]
      (reduce extension (map (fn [k] alfabet) (range 0 n))))


(construct-words-function '(a b c d) 3)
