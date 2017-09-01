
(defn li-partition [elem some-list]
  (loop [s-li some-list fp '() sp '()]
    (if (empty? s-li)
        (list fp sp)
        (let [[x & xs] s-li]
          (if (< x elem)
              (recur (rest s-li) (cons x fp) sp)
              (recur (rest s-li) fp (cons x sp)))))))

(defn quick-sort [some-list]
  (if (<= (count some-list) 1)
      some-list
      (let [[x & xs] some-list
            [fp sp] (li-partition x xs)]
         (concat (quick-sort fp)
                 (list x)
                 (quick-sort sp)))))

;;Complexity: O(nlogn)-average-case 0(n^2)-worst case
