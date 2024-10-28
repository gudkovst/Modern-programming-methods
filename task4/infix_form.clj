(ns task4.infix-form
  (:use task4.definitions)
  (:use task4.transformations))


; константа, переменная -> терм
; / терм -> терм
; (терм op терм) -> терм
; op from {+ * >}


(def standard-ops
  {'+ 'disjunction,
   '* 'conjunction,
   '> 'implication,
   '/ 'ngt})


(declare infix-form-process)


(defn- infix-list-process [list-form]
  (let [[head-form [op & tail-form]] (split-with (comp not (partial contains? standard-ops)) list-form)]
    ;(println head-form op tail-form)
    (cons (standard-ops op)
          (if (empty? head-form) (list (infix-form-process tail-form))
                                 (list (apply infix-form-process head-form) (apply infix-form-process tail-form))))))


(defn- infix-form-process [form]
  (cond (number? form) (list 'constant form)
        (keyword? form) (list 'variable form)
        (list? form) (infix-list-process form)))


(defmacro infix-form [form]
  `(do ~(infix-form-process form)))
