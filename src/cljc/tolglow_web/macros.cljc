(ns tolglow-web.macros)


(defmacro ->cond-> "cond-> but also through test clauses!"
  [expr & clauses]
  (assert (even? (count clauses)))
  (let [g (gensym)
        steps (map (fn [[test step]]
                     `(if (-> ~g ~test)
                        (-> ~g ~step)
                        ~g))
                   (partition 2 clauses))]
    `(let [~g ~expr
           ~@(interleave (repeat g) (butlast steps))]
       ~(if (empty? steps)
          g
          (last steps)))))


(defmacro cond->as "cond-> all up in your as->"
  [expr name & clauses]
  (assert (even? (count clauses)))
  (let [g (gensym)
        steps (map (fn [[test step]]
                     `(if ~test
                        (-> ~g ~step)
                        ~g))
                   (partition 2 clauses))]
    `(let [~name ~expr
           ~@(interleave (repeat g)
                         (butlast steps))]
       ~(if (empty? steps)
          g
          (last steps)))))

(defmacro as-> "Binds name to expr, evaluates the first form in the lexical context
  of that binding, then binds name to that result, repeating for each
  successive form, returning the result of the last form."
  [expr name & forms]
  `(let [~name ~expr
         ~@(interleave (repeat name)
                       (butlast forms))]
     ~(if (empty? forms)
        name
        (last forms))))

()
