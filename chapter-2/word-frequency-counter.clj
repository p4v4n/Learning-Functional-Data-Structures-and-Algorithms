;;basically implementing the 'frequencies' function

;;official implementation for frequencies function
(defn word-frequency-counter [some-coll]
  (persistent! (reduce (fn [counts x]
                         (assoc! counts x (inc (get counts x 0)))) 
                       (transient {}) 
                       some-coll)))


;;using normal recursion
(defn word-frequency-counter2 [some-coll]
  (loop [s-li some-coll counts {}]
    (if (empty? s-li)
        counts
        (recur (rest s-li) 
               (assoc counts (first s-li) (inc (get counts (first s-li) 0)))))))
