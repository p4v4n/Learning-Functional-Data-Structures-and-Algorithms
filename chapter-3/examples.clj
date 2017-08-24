;;Head fo the list----

(defn get-head [some-list]
  (let [[x & _] some-list]
    x))
;;Complexity : O(1)

;;Tail(Rest) of the list-----

(defn get-tail [some-list]
  (let [[_ & xs] some-list]
    xs))
;;Complexity : O(1)

;;The above examples doesn't need checks for empty lists as destructuring in clojure 
;; takes care of those.

;;Dropping the first n elements in a list-----

(defn drop-n [n some-list]
  (if (<= n 0)
      some-list
      (let [[x & xs] some-list]
        (drop-n (dec n) xs))))
;;Complexity : O(n) (might have to traverse the entire list in the worst case)

(defn drop-while [some-list predicate-fn]
  (let [[x & xs] some-list]
    (if (and x (predicate-fn x))
        (drop-while xs predicate-fn)
        some-list)))
;;Complexity : O(n)

;;Appending Lists

(defn append-lists [list1 list2]
  (let [[x & xs] list1]
    (if x
       (conj (append-lists xs list2) x)
       list2)))
;;Complexity: O(n) Proportional to the length of the list being copied


