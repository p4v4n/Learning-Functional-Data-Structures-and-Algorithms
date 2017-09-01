;;Insertion Sort

(defn insert-element [element sorted-list]
  (if (empty? sorted-list)
      (list element)
      (let [[x & xs] sorted-list]
        (if (<= x element)
            (cons x (insert-element element xs))
            (cons element sorted-list)))))

(defn insertion-sort [some-list]
  (loop [s-list '() data some-list]
    (if (empty? data)
        s-list
        (recur (insert-element (first data) s-list) (rest data)))))
