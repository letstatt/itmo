; basics

(def constant constantly)

(defn variable [name]
  (fn [vars] (vars name)))

(defn create-operation [operator]
  (fn [& args]
    (fn [vars]
      (apply operator (mapv #(% vars) args)))))

; operations

(def add (create-operation +))
(def negate (create-operation -))
(def subtract (create-operation -))
(def multiply (create-operation *))
(def divide (create-operation
              (fn
                ([arg] (/ 1 (double arg)))
                ([arg & args] (reduce #(/ (double %1) (double %2)) arg args)))))

(def avg (create-operation
           (fn [& args] (/ (apply + args) (count args)))))

(def sum add)

(def operations
  {'+      add
   '-      subtract
   '*      multiply
   '/      divide
   'negate negate
   'avg    avg
   'sum    add
   })

; parse time

(defn parseFunction [expr]
  (letfn [(reduce-expr [token]
            (cond
              (number? token) (constant token)
              (symbol? token) (variable (str token))
              (seq? token) (apply (get operations (first token)) (mapv reduce-expr (rest token)))))]
    (reduce-expr (read-string expr))))
