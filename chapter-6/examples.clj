;;Reversing the list
(defn slow-rev [some-list]
  (if (empty? some-list)
      '()
      (let [[x & xs] some-list]
        (concat (slow-rev xs) (list x)))))

(defn rev [some-list]
  (loop [li some-list acc '()]
    (if (empty? li)
        acc
        (let [[x & xs] li]
          (recur xs (cons x acc))))))

;;Graph Algorithms

(def some-graph 
    (list ["m" "n"] ["m" "o"] ["m" "p"] ["n" "q"] 
          ["o" "r"] ["p" "q"] ["q" "r"] ["q" "s"]))

;;finds the set of successors of a given vertex
(defn succ-set [some-str some-list]
      (->> some-list
           (filter #(= some-str (first %)))
           (map second)))

;;Graph Traversal (depth-first and breadth-first)

;;Depth-first Traversal
(defn depthf [nodes visited graph]
  (if (empty? nodes)
      visited
      (let [[x & xs] nodes]
        (if (contains? (set visited) x)
            (depthf xs visited graph)
            (depthf (concat (succ-set x graph) xs)
                   (cons x visited) graph)))))

(defn depth-first [initial-string graph]
  (reverse (depthf (list initial-string) '() graph)))


;; Breadth-First Traversal (very simlar imilar to dept-first)
(defn breadthf [nodes visited graph]
  (if (empty? nodes)
      visited
      (let [[x & xs] nodes]
        (if (contains? (set visited) x)
            (breadthf xs visited graph)
            (breadthf (concat xs (succ-set x graph))
                   (cons x visited) graph)))))

(defn breadth-first [initial-string graph]
  (reverse (breadthf (list initial-string) '() graph)))

;;Topological Sorting

(def second-graph (list ["getup" "shower"] ["shower" "breakfast"] ["breakfast" "dress"] ["dress" "office"] 
          ["office" "dinner"] ["breakfast" "leisurely_lunch"] ["leisurely_lunch" "movie"] ["movie" "dinner"]))

(defn sort [nodes visited graph]
  (if (empty? nodes)
      visited
      (let [[x & xs] nodes]
        (if (contains? (set visited) x)
            (sort xs visited graph)
            (sort xs (cons x (sort (succ-set x graph) visited graph))
                          graph)))))
(defn topsort [graph]
  (sort (list (first (first graph))) '() graph))
