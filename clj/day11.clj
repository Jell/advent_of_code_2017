(require '[clojure.string :as str])

(defn str->move [s]
  (case s
    "n"  [   0    1]
    "nw" [-1/2  1/2]
    "ne" [ 1/2  1/2]
    "sw" [-1/2 -1/2]
    "se" [ 1/2 -1/2]
    "s"  [   0   -1]))

(def moves
  (map str->move
       (str/split (str/trim (slurp "day11_input.txt")) #",")))

;; Part 1
(apply + (reduce (fn [[a1 b1] [a2 b2]]
                   [(+ a1 a2) (+ b1 b2)])
                 moves))

;; Part 2
(apply max
       (map (partial apply +)
            (reductions (fn [[a1 b1] [a2 b2]]
                          [(+ a1 a2) (+ b1 b2)])
                        moves)))
