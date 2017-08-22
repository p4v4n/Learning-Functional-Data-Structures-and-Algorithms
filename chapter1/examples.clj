;;Printing all elements in a list

(defn print-each-element [some-li]
  (map println some-li))

(defn print-each-element2 [some-li]
  (loop [li some-li]
  (when (not-empty li)
        (println (first li))
        (recur (rest li)))))

;;Higher level abstractions

;example
((juxt (comp str *) (comp str +)) 1 2 3 4 5)

;;Counting the number of even elements in a list

(defn count-even-elements [some-li]
  (->> some-li
       (filter even?)
       count))

;;Generating combinations of values from two lists

(defn fun1 [list1 list2]
  (for [x list1 y list2]
    (list x y)))

;;Partitioning a list into 2 lists containing even and odd elements

(defn partition-even-odd [some-li]
  ((juxt filter remove) even? some-li))

;;Multiple each element of a vector by two

(defn multiply-each-by-two [some-li]
  (map #(* 2 %) some-li))

(defn multiply-each-by-two2 [some-li]
  (map * (repeat 2) some-li))

;;Product of the first-n positive integers

(defn product-till-n [n]
  (apply * (range 1 (inc n))))

;;Factorial function with tail call optimization

(defn factorial [n]
  (loop [x n acc 1]
    (if (= x 0)
        acc
        (recur (dec x) (* x acc)))))

;; write a program to generate numbers from 1 to 100. 
;We wish to check which numbers are evenly divisible by 2, 3, 4, and 5.

(defn divisible-by-x [x n]
  (println (str "checking " n " by " x))
  (= 0 (rem n x)))

(def by2 (partial divisible-by-x 2))
(def by3 (partial divisible-by-x 3))
(def by4 (partial divisible-by-x 4))
(def by5 (partial divisible-by-x 5))

(defn divisible-by-2345 [n]
  (->> n
      ((juxt by2 by3 by4 by5))
      (every? true?)))

(defn evenly-divisible-numbers [n]
  (->> (range 1 (inc n))
       (filter divisible-by-2345)))
;--needs optimization(shd apply by5 only when the previous ones are true)-------

;;weed out zeros from a vector

(defn weed-out-zeros [some-li]
  (filter (complement zero?) some-li))

;;given a sequence check if it is strictly increasing

(defn strictly-increasing? [some-li]
  (let [x-li some-li
        y-li (rest some-li)]
    (->> (map < x-li y-li)
         (every? true?))))

;---second way

(defn check? [some-li]
  (let [[x y] some-li]
    (< x y)))

(defn gen-pairs [some-li]
  (let [x some-li
        y (rest some-li)]
    (->> (interleave x y)
         (partition 2))))

(defn strictly-increasing2? [some-li]
  (->> (gen-pairs some-li)
       (every? check?)))
