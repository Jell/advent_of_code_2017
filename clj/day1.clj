(require [clojure.string :as str])

(def input (str/trim (slurp "day1_input.txt")))

;; Part 1
(reduce + (map (fn [a b]
                 (if (= a b)
                   (read-string (str a))
                   0))
               input
               (drop 1 (cycle input))))


;; Part 2
(reduce + (map (fn [a b]
                 (if (= a b)
                   (read-string (str a))
                   0))
               input
               (drop (/ (count input) 2) (cycle input))))
