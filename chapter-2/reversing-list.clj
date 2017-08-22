
(defn reverse-list [some-li]
  (if (empty? some-li)
      '()
      (concat (reverse-list (rest some-li)) (vector (first some-li)))))
;;not the idiomatic way to do it
;Complexity -> O(n^2) because adding a element to the tail of a linked-list is O(N)


;;Using a higher-order-function to improve performance

(defn reverse-list2 [some-li]
  (reduce conj '() some-li))

;Complexity -> O(N)
