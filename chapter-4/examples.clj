;;Building a binary-tree from a list.
(defn build-tree [some-list]
  (if (empty? some-list)
      "Leaf"
      (let [[x & xs] some-list
            k (count xs)
            ks (quot k 2)]
        (list x (build-tree (take ks xs)) (build-tree (drop ks xs))))))

;; Size of the binary-tree(number of non-leafy nodes)
(defn size-build-tree [binary-tree]
  (if (= binary-tree "Leaf")
      0
      (let [[_ l-tree r-tree] binary-tree]
        (+ 1 (size-build-tree l-tree) (size-build-tree r-tree)))))

;;Depth of the binary-tree
(defn depth-build-tree [binary-tree]
    (if (= binary-tree "Leaf")
      0
      (let [[_ l-tree r-tree] binary-tree]
        (+ 1 (max (depth-build-tree l-tree) (depth-build-tree r-tree))))))

;;Generating a complete binary tree given initial-node and depth
(defn build-complete-tree [v depth]
  (if (zero? depth)
      "Leaf"
      (list v (build-complete-tree (* 2 v) (dec depth)) 
              (build-complete-tree (+ 1 (* 2 v)) (dec depth)))))

;;Compare two trees to check if they are equal or not
(defn trees-equal?[tree1 tree2]
  (if (and (= tree1 "Leaf") (= tree2 "Leaf"))
      true
      (if (or (= tree1 "Leaf") (= tree2 "Leaf"))
          false
          (let [[v1 l1 r1] tree1
                [v2 l2 r2] tree2]
            (and (= v1 v2) (trees-equal? l1 l2) (trees-equal? r1 r2))))))

;;Flipping a binary tree
(defn flip-tree[binary-tree]
  (if (= binary-tree "Leaf")
      "Leaf"
      (let [[v l r] binary-tree]
        (list v (flip-tree r) (flip-tree l)))))

;;Compare if one tree is a flipped-tree of the other
(defn flipped-equal?[tree1 tree2]
  (if (and (= tree1 "Leaf") (= tree2 "Leaf"))
      true
      (if (or (= tree1 "Leaf") (= tree2 "Leaf"))
          false
          (let [[v1 l1 r1] tree1
                [v2 l2 r2] tree2]
            (and (= v1 v2) (flipped-equal? l1 r2) (flipped-equal? l2 r1))))))

;;Binary tree traversal
;pre-order: root,left,right
;in-order: left,root,right
;post-order: left,right,root

(defn pre-order[binary-tree]
  (if (= binary-tree "Leaf")
      nil
      (let [[v l r] binary-tree]
        (concat (list v) (pre-order l) (pre-order r)))))

(defn in-order[binary-tree]
  (if (= binary-tree "Leaf")
      nil
      (let [[v l r] binary-tree]
        (concat (in-order l) (list v) (in-order r)))))

(defn post-order[binary-tree]
  (if (= binary-tree "Leaf")
      nil
      (let [[v l r] binary-tree]
        (concat (post-order l) (post-order r) (list v)))))

;;traversal with accumulator

(defn pre-order-acc [binary-tree acc]
  (if (= binary-tree "Leaf")
      acc
      (let [[v l r] binary-tree]
        (cons v (pre-order-acc l (pre-order-acc r acc))))))

(defn in-order-acc [binary-tree acc]
  (if (= binary-tree "Leaf")
      acc
      (let [[v l r] binary-tree]
        (in-order-acc l (cons v (in-order-acc r acc))))))

(defn post-order-acc [binary-tree acc]
  (if (= binary-tree "Leaf")
      acc
      (let [[v l r] binary-tree]
        (post-order-acc l (post-order-acc r (cons v acc))))))

;;Binary-Search-Trees
;;self-exercise: create a BST from a dict with strings as keys and numbers a values

;;Node-Insertion to a BST
(defn node-insertion [k v bst]
  (if (= bst "Leaf")
      (list [k v] "Leaf" "Leaf")
      (let [[[ke va] l r] bst]
        (cond
          (= k ke) "Error"
          (< k ke) (list [ke va] (node-insertion k v l) r)
          (> k ke) (list [ke va] l (node-insertion k v r))))))

;;Searching for a Key
(defn search-key [k bst]
  (if (= bst "Leaf")
      "None"
      (let [[[ke va] l r] bst]
        (cond
          (= k ke) va
          (< k ke) (search-key k l)
          (> k ke) (search-key k r)))))

;;Updatng the value of a Key
(defn node-update [k v bst]
  (if (= bst "Leaf")
      (list [k v] "Leaf" "Leaf")
      (let [[[ke va] l r] bst]
        (cond
          (= k ke) (list [ke v] l r)
          (< k ke) (list [ke va] (node-update k v l) r)
          (> k ke) (list [ke va] l (node-update k v r))))))
