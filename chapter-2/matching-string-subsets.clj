;Input: A string s and a dictionary of words, represented by dict
;Output: Break the string into those words that are provided in the given dictionary.

;;using recursive backtracking

(defn break-into-words [some-str some-dict]
  (if (contains? some-dict some-str)
      some-str
      (loop [ind 1]
        (if (= ind (count some-str))
            nil
            (let [first-match (subs some-str 0 ind)
                  rest-match (subs some-str ind)]
              (if (and (contains? some-dict first-match) 
                       (break-into-words rest-match some-dict))
                  (str first-match " " (break-into-words rest-match some-dict))
                  (recur (inc ind))))))))

;;Complexity -> O(N^2)
