(require '[clojure.test :refer [deftest are run-all-tests]])

(comment
  17  16  15  14  13
  18   5   4   3  12
  19   6   1   2  11
  20   7   8   9  10
  21  22  23
  )

(def dirs (cycle [:right :up :left :down]))

(defn coords-iter [target idx [dir & next-dirs :as dirs] [x y]]
  (if (= target idx)
    [x y]
    (case dir
      :right (recur target (inc idx) (if (= x (Math/abs y)) next-dirs dirs) [(inc x) y])
      :up    (recur target (inc idx) (if (= x (inc y))      next-dirs dirs) [x (inc y)])
      :left  (recur target (inc idx) (if (= (dec x) (- y))  next-dirs dirs) [(dec x) y])
      :down  (recur target (inc idx) (if (= x (dec y))      next-dirs dirs) [x (dec y)]))))

(defn coords [n]
  (coords-iter n 1 dirs [0 0]))

(deftest coords-test
  (are [x c] (= c (coords x))
    1  [ 0  0]
    2  [ 1  0]
    3  [ 1  1]
    4  [ 0  1]
    5  [-1  1]
    6  [-1  0]
    7  [-1 -1]
    8  [ 0 -1]
    9  [ 1 -1]
    10 [ 2 -1]
    11 [ 2  0]
    12 [ 2  1]
    13 [ 2  2]
    14 [ 1  2]
    15 [ 0  2]
    16 [-1  2]
    17 [-2  2]
    18 [-2  1]
    19 [-2  0]
    20 [-2 -1]
    21 [-2 -2]
    22 [-1 -2]
    23 [ 0 -2]))

(run-all-tests #"coords-test")

(defn dist [[x y]]
  (+ (Math/abs x)
     (Math/abs y)))

(dist (coords 361527))

(reduce (fn [vals idx]
          (let [[x y] (coords idx)
                v (reduce +' (for [dx (range -1 2) dy (range -1 2)]
                               (get vals [(+' x dx) (+' y dy)] 0)))]
            (if (> v 361527)
              (reduced v)
              (assoc vals [x y] v))))
        {[0 0] 1}
        (range 2 361527))
