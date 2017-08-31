;;Note that we need to write the numbers 
;;such that the least significant bit is on the left-hand side.
;;Incrementing a binary number
(defn increment [num-list]
  (if (empty? num-list)
      '(1)
      (let [[x & xs] num-list]
        (case x
          0 (cons 1 xs)
          1 (cons 0 (increment xs))))))

;;Adding 2 binary numbers
(defn add-binary [num1 num2]
  (cond
    (and (empty? num1) (empty? num2)) '()
    (empty? num1) num2
    (empty? num2) num1
    :else (do (let [[x1 & xs1] num1
                    [x2 & xs2] num2]
                (cond 
                  (zero? x1) (cons x2 (add-binary xs1 xs2))
                  (zero? x2) (cons x1 (add-binary xs1 xs2))
                  :else (cons 0 (increment (add-binary xs1 xs2))))))))

;;Skipping the implementation of linked lists as binary trees for now
