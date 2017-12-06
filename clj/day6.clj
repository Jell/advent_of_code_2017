(require '[clojure.test :refer [deftest is are run-all-tests]])

(def blocks [4 1 15 12 0 9 9 5 5 8 7 3 14 5 12 3])

(defn step [blocks]
  (let [l (count blocks)
        m (apply max blocks)
        start (.indexOf blocks m)]
    (loop [i m idx start bs (assoc blocks start 0)]
      (if (zero? i)
        bs
        (let [next-idx (mod (inc idx) l)]
          (recur (dec i) next-idx (update bs next-idx inc)))))))

(defn first-dup [bs]
  (loop [idx 0 seen #{} [block & next-blocks] bs]
    (if (seen block)
      [idx block]
      (recur (inc idx) (conj seen block) next-blocks))))

(deftest step-test
  (are [before _ after] (= after (step before))
    [0 2 7 0] => [2 4 1 2]
    [2 4 1 2] => [3 1 2 3]
    [3 1 2 3] => [0 2 3 4]
    [0 2 3 4] => [1 3 4 1]
    [1 3 4 1] => [2 4 1 2]))

(run-all-tests #"step-test")

;; Part 1
(def part1
  (first-dup (iterate step blocks)))

;; Part 2
(let [[second-idx dup] part1
      first-idx (.indexOf (iterate step blocks) dup)]
  (- second-idx first-idx))
