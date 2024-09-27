(ns task2.t2-2-test
  (:use task2.t2-2)
  (:require [clojure.test :as test]))


(test/deftest task2.t2-2-test
  (test/testing "Testing task2-2"
    (test/is (= ((integrate sin) 0) 0.0))
    (test/is (= ((integrate (fn [x] 5)) 6) 30.0))
    (test/is (<= ((integrate sin) Math/PI) 2))
    (test/is (= "incorrect argument" ((integrate sin) -1)))))
