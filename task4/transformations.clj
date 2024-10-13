(ns task4.transformations
  (:use task4.definitions))

(declare dnf)


(defn- rem-par "remove parenthesis in inner conjunctions (return list of args)" [expr]
  (cond (disjunction? expr) (list (dnf expr))
        (conjunction? expr) (reduce concat (map rem-par (args expr)))
        :default (list expr)))

(defn conj-simplification "remove parenthesis in inner conjunctions and duplicate of vars" [expr]
  (let [arg (rem-par expr)
        vars (map variable (apply hash-set (map variable-name (filter variable? arg))))
        others (remove variable? arg)]
    (apply conjunction (concat vars others))))


(defn distributivity "apply distributivity" [conjuncts disjuncts]
  (cond (and (empty? conjuncts) (empty? disjuncts)) (list)
        (empty? disjuncts) (apply disjunction (map conj-simplification conjuncts))
        :default (distributivity (reduce concat (map #(map (fn [c] (conjunction c %)) conjuncts) (args (first disjuncts))))
                                 (rest disjuncts))))


(def dnf-rules (list
                 [#(ngt? %) #(cond
                               (disjunction? (args %)) (dnf (apply conjunction (map ngt (args (args %)))))
                               (conjunction? (args %)) (dnf (apply disjunction (map ngt (args (args %)))))
                               (constant? (args %)) (constant (- 1 (constant-value %)))
                               :default %)]

                 [#(disjunction? %) (fn [expr] (let [args-dnf (map #(dnf %) (args expr))
                                                     disjuncts (->> args-dnf
                                                                    (filter disjunction?)
                                                                    (map args)
                                                                    (reduce concat))
                                                     others (remove disjunction? args-dnf)]
                                                 (apply disjunction (concat others disjuncts))))]

                 [#(conjunction? %) (fn [expr] (let [simple-conj (conj-simplification expr)
                                                     vars (filter ngt-variable? (args simple-conj))
                                                     conjuncts (if (empty? vars) (list) (list (apply conjunction vars)))
                                                     disjuncts (filter disjunction? (args simple-conj))]
                                                 (distributivity conjuncts disjuncts)))]
                 [(fn [_] true) (fn [expr] expr)]
                 ))


(defn dnf "adduction expr to disjunction normal form" [expr]
  ((some (fn [rule] (if ((first rule) expr) (second rule) false)) dnf-rules) expr))
