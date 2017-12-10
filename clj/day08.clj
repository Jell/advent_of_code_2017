(require '[clojure.string :as str])

(def input
  (-> (slurp "day08_input.txt")
      (str/split #"\n")))

(defn parse [line]
  (let [[target op-str op-val-str _ cond-target cond-op-str cond-val-str]
        (str/split line #" ")

        op-val (read-string op-val-str)

        op (case op-str
             "inc" (fnil (partial + op-val) 0)
             "dec" (fnil (partial + (- op-val)) 0))

        cond-op (case cond-op-str
                  ">" >
                  "<" <
                  ">=" >=
                  "<=" <=
                  "==" =
                  "!=" not=)

        cond-val (read-string cond-val-str)]
    (fn [state]
      (update state target
              (fn [val] (if (cond-op (get state cond-target 0) cond-val)
                          (op val)
                          (or val 0)))))))

;; Part 1
(->> (map parse input)
     (reduce (fn [state step] (step state)) {})
     vals
     (apply max))

;; Part 2
(->> (map parse input)
     (reductions (fn [state step] (step state)) {})
     (mapcat vals)
     (apply max))
