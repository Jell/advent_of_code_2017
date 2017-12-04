(require '[clojure.test :refer [deftest is run-all-tests]])

(comment
  17  16  15  14  13
  18   5   4   3  12
  19   6   1   2  11
  20   7   8   9  10
  21  22  23
  )

(def target 361527)

;; Part 1
(def dirs (cycle [:right :up :left :down]))

(defn coords-iter [[dir & next-dirs :as dirs] [x y]]
  (lazy-seq
   (cons [x y]
         (case dir
           :right (coords-iter (if (= x (- y))       next-dirs dirs) [(inc x) y])
           :up    (coords-iter (if (= x (inc y))     next-dirs dirs) [x (inc y)])
           :left  (coords-iter (if (= (dec x) (- y)) next-dirs dirs) [(dec x) y])
           :down  (coords-iter (if (= x (dec y))     next-dirs dirs) [x (dec y)])))))

(def coords (coords-iter dirs [0 0]))

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
