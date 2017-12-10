(require '[clojure.string :as str])
(require '[clojure.walk :as walk])
(require '[clojure.pprint :as pp :refer [pprint]])

(def input (str/split (slurp "day07_input.txt")
                      #"\n"))

(defn parse [line]
  (let [[node-str children-str] (str/split line #" -> ")
        [_ node-name node-weight-str] (re-find #"(\w+) \((\d+)\)" node-str)]
    [node-name (read-string node-weight-str)
     (if children-str
       (str/split children-str #", ")
       [])]))

(defn make-tree [deps]
  (loop [[[node weight children-keys :as entry] & others] deps trees {}]
    (if node
      (if (seq children-keys)
        (if (every? (set (keys trees)) children-keys)
          (recur others
                 (let [children (into {} (map (juxt identity (partial get trees))
                                              children-keys))

                       trees (apply dissoc trees children-keys)]
                   (assoc trees node {:weight weight :children children})))
          (recur (conj (vec others) entry) trees))
        (recur others (assoc trees node {:weight weight :children nil})))


      trees)))

(def tree (make-tree (map parse input)))

;; Part 1
(-> tree keys first)

;; Part 2
(walk/postwalk (fn [x]
                 (if (and (map? x)
                          (contains? x :weight))
                   (let [{:keys [weight children]} x]
                     (if (or (nil? children)
                             (apply = (map :sum (vals children))))
                       (assoc x :sum (+ weight
                                        (reduce + (map :sum (vals children)))))
                       (throw (ex-info (str "Imbalanced: "
                                            (mapv (juxt :weight :sum) (vals children)))
                                       {}))))
                   x)) tree)
