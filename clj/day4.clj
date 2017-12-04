(require '[clojure.string :as str])

(def input (slurp "day4_input.txt"))

;; Part 1
(->> (str/split input #"\n")
     (map #(str/split % #" "))
     (map frequencies)
     (map vals)
     (filter (partial every? #{1}))
     count)

;; Part 2
(->> (str/split input #"\n")
     (map #(str/split % #" "))
     (map (partial map sort))
     (map frequencies)
     (map vals)
     (filter (partial every? #{1}))
     count)
