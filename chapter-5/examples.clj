;;Carry Operation
(defn carry [c binary-li]
  (case c
    0 binary-li
    1 (if (empty? binary-li)
          '(1)
          (let [[x & xs] binary-li]
            (cons (- 1 x) (carry x xs))))
    "Invalid Input!!"))

;;Adding binary numbers

(defn add [c ps qs]
  (cond
    (and (empty? ps) (empty? qs)) (carry c '())
    (empty? ps) (carry c qs)
    (empty? qs) (carry c ps)
    :else (let [[x & xs] ps
                [y & ys] qs]
            (cons (rem (+ c x y) 2) (add (quot (+ c x y) 2) xs ys)))))

(defn add-nums [f-list s-list]
  (reverse (add 0 (reverse f-list) (reverse s-list))))

;;Multiplying binary numbers

(defn multiply [ps qs]
  (if (empty? ps)
      nil
      (let [[x & xs] ps]
        (case x
          0 (cons 0 (multiply xs qs))
          1 (add 0 qs (cons 0 (multiply xs qs)))))))

(defn mult [f-list s-list]
  (reverse (multiply (reverse f-list) (reverse s-list))))

;;Greedy Algorithms

;;Coin denomination problem
(defn greedy-change [denom amount]
  (if (zero? amount)
      nil
      (let [[x & xs] denom]
        (if (> x amount)
            (greedy-change xs amount)
            (cons x (greedy-change denom (- amount x)))))))
;;can result in error sometimes ex:denom [5 2] amount 16

;;with backtracking
(defn bt-changes [result denom amount]
  (if (zero? amount)
       result
       (if (empty? denom)
           '()
           (let [[x & xs] denom]
             (if (neg? amount)
                 '()
                 (remove empty?
                         (list (bt-changes (cons x result) denom (- amount x))
                       (bt-changes result xs amount))))))))
;;returns inside nests(needs modification)
