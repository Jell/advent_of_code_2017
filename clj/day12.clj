(require '[clojure.string :as str])
(require 'clojure.set)

(def chain (slurp "day12_input.txt"))

(def connections
  (reduce
   (fn [accu x]
     (let [[x xs] (str/split x #" <-> ")]
       (assoc accu x (set (str/split xs #", ")))))
   {}
   (str/split-lines chain)))

(defn connected [conns start]
  (loop [link start]
    (let [new-link (reduce clojure.set/union
                           link
                           (vals (select-keys conns link)))]
      (if (= new-link link)
        new-link
        (recur new-link)))))

;; Part 1
(count (connected connections #{"0"}))

;; Part 2
(count
 (loop [groups [] conns connections]
   (if (seq conns)
     (let [seed (-> conns keys first vector set)
           group (connected conns seed)]
       (recur (conj groups group)
              (apply dissoc conns group)))
     groups)))
