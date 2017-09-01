
;;gives the largest number from a list and the rest of the list in original order
(defn get-largest [some-list]
  (if (empty? some-list) 
      (list nil '())
      (let [[x & xs] some-list]
        (if (nil? xs)
            (list x '())
            (let [[y ys] (get-largest xs)]
              (if (>= x y)
                  (list x (cons y ys))
                  (list y (cons x ys))))))))

(defn bubble-sort [some-list]
  (if (empty? some-list)
      nil
      (let [[x xs] (get-largest some-list)]
        (concat (bubble-sort xs) (list x)))))

;;stable-sorting algorithm
;complexity: O(n^2)
