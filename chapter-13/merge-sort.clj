
(defn split [some-list]
  (if (empty? some-list)
      '(nil nil)
      (let [[x & xs] some-list]
        (if (empty? xs)
            (list (list x) nil)
            (let [[y & ys] xs
                  [p q] (split ys)]
              (list (cons x p) (cons y q)))))))

(defn merge-sl[lst1 lst2]
  (cond 
    (empty? lst1) lst2
    (empty? lst2) lst1
    :else (do 
            (let [[x & xs] lst1
                  [y & ys] lst2]
              (if (< x y)
                  (cons x (merge-sl xs lst2))
                  (cons y (merge-sl lst1 ys)))))))

(defn merge-sort [some-list]
  (if (<= (count some-list) 1)
      some-list
      (let [[x y] (split some-list)
            p (merge-sort x)
            q (merge-sort y)]
        (merge-sl p q))))

;;Complexity : O(nlogn)
