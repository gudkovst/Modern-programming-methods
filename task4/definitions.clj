(ns task4.definitions)


(defn args "get arguments of expr" [expr]
  (rest expr))


(defn constant "create boolean constant {0, 1}" [num]
  {:pre [(or (= num 0) (= num 1))]}
  (list ::const num))

(defn constant? "check that expr is constant" [expr]
  (= (first expr) ::const))

(defn constant-value "get value of constant" [expr]
  {:pre [(constant? expr)]}
  (second expr))


(defn ngt? "check that expr is negation" [expr]
  (= (first expr) ::not))

(defn ngt "create negation of expr with remove double negation" [expr]
  (if (ngt? expr) (args expr) (cons ::not expr)))


(defn variable "create boolean variable (as :name)" [name]
  {:pre [(keyword? name)]}
  (list ::var name))

(defn variable? "check that expr is variable" [expr]
  (= (first expr) ::var))

(defn ngt-variable? "check that expr is variable or negation of variable" [expr]
  (or (variable? expr) (and (ngt? expr) (variable? (args expr)))))

(defn variable-name "get name of variable" [v]
  {:pre [(variable? v)]}
  (second v))

(defn same-variables? "check that v1 and v2 are same variable" [v1 v2]
  (and
    (variable? v1)
    (variable? v2)
    (= (variable-name v1) (variable-name v2))))


(defn disjunction "create disjunction of finite number exprs" [expr & rest]
  (if (empty? rest) expr (cons ::disj (cons expr rest))))

(defn disjunction? "check that expr is disjunction" [expr]
  (= (first expr) ::disj))


(defn conjunction? "check that expr is conjunction" [expr]
  (= (first expr) ::conj))

(defn conjunction "create conjunction of finite number exprs with reduction constants" [expr & rest]
  (if (empty? rest) expr
                    (let [arg (cons expr rest)
                          c (reduce * (map constant-value (filter constant? arg)))
                          others (remove constant? arg)]
                      (cond (= c 0) (constant 0)
                            (empty? others) (constant c)
                            :default (cons ::conj others)))))


(defn implication "create implication of finite number exprs (expr1 -> (expr2 -> ...))" [expr & rest]
  (if (empty? rest) expr
                    (disjunction (ngt expr) (apply implication rest))))

(defn implication? "check that expr is implication" [expr]
  (and (= (first expr) ::disj) (ngt? (first (args expr)))))
