(ns task4.task4-test
  (:use task4.definitions)
  (:use task4.transformations)
  (:require [clojure.test :as test]))


(def expr (variable :x))

(test/deftest task4.definitions-test
  (test/testing "Testing task4.definitions"
    (test/is (constant? (constant 1)))
    (test/is (= 1 (constant-value (constant 1))))

    (test/is (variable? (variable :x)))
    (test/is (= :x (variable-name (variable :x))))
    (test/is (same-variables? (variable :x) (variable :x)))
    (test/is (not (same-variables? (variable :x) (variable :y))))

    (test/is (ngt? (ngt expr)))
    (test/is (not (ngt? (ngt (ngt expr)))))

    (test/is (disjunction? (disjunction expr expr expr)))

    (test/is (conjunction? (conjunction expr expr (disjunction expr expr))))
    (test/is (constant? (conjunction (constant 1) (constant 1) (constant 0))))
    (test/is (= 0 (constant-value (conjunction (constant 0) expr))))

    (test/is (implication? (implication expr expr expr)))

    (test/is (not (disjunction? (conjunction expr expr))))
    (test/is (not (ngt? (implication expr (ngt? expr)))))
    ))


(test/deftest task4.transformations-test
  (test/testing "Testing task4.transformations"
    (test/is (= 2 (count (args (conj-simplification (conjunction (variable ::x)
                                                                 (conjunction (variable ::y)
                                                                              (conjunction (variable ::y) (constant 1) (constant 1)))))))))
    (test/is (= 4 (count (args (dnf (conjunction (variable ::x) (variable ::y) (disjunction (variable ::a)
                                                                                            (conjunction (variable ::b) (variable ::c)))
                                                 (disjunction (variable ::a) (variable ::d))))))))
    (test/is (= (dnf (ngt (disjunction (variable ::x) (variable ::y))))
                (conjunction (ngt (variable ::x)) (ngt (variable ::y)))))
    (test/is (= 4 (count (args (dnf (implication (variable ::x) (variable ::y)
                                                 (disjunction (variable ::q) (variable ::w))))))))
    ))
