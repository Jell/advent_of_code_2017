(require '[clojure.test :refer [deftest is run-all-tests]])

(comment
  17  16  15  14  13
  18   5   4   3  12
  19   6   1   2  11
  20   7   8   9  10
  21  22  23   →
  )

(def target 361527)

;; Part 1
(def dirs (cycle [:→ :↑ :← :↓]))

(defn coords-iter [[dir & next :as dirs] [x y]]
  (lazy-seq
   (cons [x y]
         (case dir
           :→ (coords-iter (if (= x (- y))       next dirs) [(inc x) y])
           :↑ (coords-iter (if (= x (inc y))     next dirs) [x (inc y)])
           :← (coords-iter (if (= (dec x) (- y)) next dirs) [(dec x) y])
           :↓ (coords-iter (if (= x (dec y))     next dirs) [x (dec y)])))))

(def coords (coords-iter dirs [0 0]))

(defn dist [[x y]]
  (+ (Math/abs x)
     (Math/abs y)))

(dist (nth coords target))

;; Part 2
(reduce (fn [vals [x y]]
          (let [v (reduce +' (for [dx (range -1 2) dy (range -1 2)]
                               (get vals [(+' x dx) (+' y dy)] 0)))]
            (if (> v target)
              (reduced v)
              (assoc vals [x y] v))))
        {[0 0] 1}
        coords)

;; Tests

(deftest coords-test
  (is (= (take 23 coords)
         [[ 0  0]
          [ 1  0]
          [ 1  1]
          [ 0  1]
          [-1  1]
          [-1  0]
          [-1 -1]
          [ 0 -1]
          [ 1 -1]
          [ 2 -1]
          [ 2  0]
          [ 2  1]
          [ 2  2]
          [ 1  2]
          [ 0  2]
          [-1  2]
          [-2  2]
          [-2  1]
          [-2  0]
          [-2 -1]
          [-2 -2]
          [-1 -2]
          [ 0 -2]])))

(run-all-tests #"coords-test")
