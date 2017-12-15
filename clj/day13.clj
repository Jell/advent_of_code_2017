(require '[clojure.string :as str])

(def input-str (slurp "day13_input.txt"))

(def init-input
  (map (fn [s]
         (let [[idx l] (str/split s #": ")]
           [(read-string idx)
            (read-string l)]))
       (str/split-lines input-str)))

(defn interrupts [state]
  (reduce + (map (fn [[idx l]]
                   (if (zero? (mod idx (* 2 (dec l))))
                     (* idx l)
                     0))
                 state)))

;; Part 1
(interrupts init-input)

;; Part 2
(defn wait [input]
  (map (fn [[k v]] [(inc k) v]) input))

(count (take-while (complement zero?)
                   (map interrupts (iterate wait init-input))))
