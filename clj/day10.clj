(require '[clojure.string :as str])

(def input (range 256))
(def lengths [165 1 255 31 87 52 24 113 0 91 148 254 158 2 73 153])

(defn lens [idx l]
  {:view
   (fn [chain]
     (let [chain-length (count chain)]
       (->> (cycle chain)
            (drop idx)
            (take l))))
   :setter
   (fn [chain sub]
     (let [chain-length (count chain)]
       (->> (cycle chain)
            (drop idx)
            (drop l)
            (lazy-cat sub)
            (take chain-length)
            (cycle)
            (drop (- chain-length idx))
            (take chain-length))))})

(defn encrypt [init-chain init-lengths]
  (loop [idx 0 skip 0 chain init-chain [l & ls] init-lengths]
    (if l
      (let [{:keys [view setter]} (lens idx l)
            sub (view chain)
            new-chain (->> (view chain)
                           reverse
                           (setter chain))]

        (recur (mod (+ idx l skip) (count chain))
               (inc skip)
               new-chain
               ls))
      chain)))

;; Part 1
(apply * (take 2 (encrypt input lengths)))

;; Part 2
(def length-str "165,1,255,31,87,52,24,113,0,91,148,254,158,2,73,153")

(def salt [17 31 73 47 23])

(def rounds 64)

(defn str->lengths [s]
  (concat (map int s) salt))

(defn compute-hash [s]
  (->> (str->lengths s)
       (repeat 64)
       (apply concat)
       (encrypt input)
       (partition 16)
       (map (partial reduce bit-xor))
       (map (partial format "%02x"))
       (str/join "")))

(println (compute-hash length-str))

;; tests
(require '[clojure.test :refer [deftest are run-all-tests]])

(deftest hash-test
  (are [before _ after] (= after (compute-hash before))
    "" => "a2582a3a0e66e6e86e3812dcb672a272"
    "AoC 2017" => "33efeb34ea91902bb2f59c9920caa6cd"
    "1,2,3" => "3efbe78a8d82f29979031a4aa0b16a9d"
    "1,2,4" => "63960835bcdc130f0b66d7ff4f6a5a8e"))

(run-all-tests #"day10.*")
