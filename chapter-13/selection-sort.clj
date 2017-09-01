
(defn index-of [item coll]
  (count (take-while (partial not= item) coll)))

(defn selection-sort [some-list]
  (if (empty? some-list)
      '()
      (let [[x & xs] some-list]
        (if (empty? xs)
            (list x)
            (let [min-val (apply min xs)]
              (if (<= x min-val)
                  (cons x (selection-sort xs))
                  (let [min-index (index-of min-val xs)]
                    (cons min-val 
                          (selection-sort (concat (take min-index xs) 
                                                  (list x) 
                                                  (drop (inc min-index) xs)))))))))))

;;stable-sorting algorithm
;complexity: O(n^2)
