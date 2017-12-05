(def input (read-string (slurp "day5_input.txt")))

(def maze-size (count input))

(defn solve [next-offset]
  (loop [steps 0 pointer 0 state (transient input)]
    (let [steps (inc steps)
          offset (get state pointer)
          new-pointer (+ pointer offset)]
      (if (<= 0 new-pointer (dec maze-size))
        (recur steps new-pointer (assoc! state pointer
                                         (next-offset offset)))
        steps))))

;; Part 1
(solve inc)

;; Part 2
(solve (fn [i]
         (if (< i 3)
           (inc i)
           (dec i))))
