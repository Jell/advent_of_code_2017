(require '[clojure.test :refer [deftest are run-all-tests]])
(require '[clojure.string :as str])

(def input (slurp "day09_input.txt"))

(defn parse [s]
  (-> (str "[" s "]") ;; wrap to make sure we have a top-level group
      (str/replace #"!." "")
      (str/replace #"<[^>]*" "<")
      (str/replace #"<>" "")
      (str/replace #"\{" "[")
      (str/replace #"\}" "]")
      read-string))

(defn depths
  ([t] (depths 0 t))
  ([depth t]
   (if (seq t)
     (reduce + depth (map (partial depths (inc depth)) t))
     depth)))

;; Part 1
(-> input parse depths)

;; Part 2
(defn remove-escape [s]
  (-> s
      (str/replace #"!." "")))

(defn remove-garbage [s]
  (-> s
      (str/replace #"!." "")
      (str/replace #"<[^>]*" "<")))

(defn count-garbage [s]
  (- (count (remove-escape s))
     (count (remove-garbage s))))

(count-garbage input)

;; Tests

(deftest parsing-test
  (are [before _ after] (= after (parse before))
    "{}" => [[]]
    "{{{}}}" => [[[[]]]]
    "{{},{}}" => [[[] []]]
    "{{{},{},{{}}}}" => [[[[] [] [[]]]]]
    "{<a>,<a>,<a>,<a>}" => [[]]
    "{{<ab>},{<ab>},{<ab>},{<ab>}}" => [[[] [] [] []]]
    "{{<!!>},{<!!>},{<!!>},{<!!>}}" => [[[] [] [] []]]
    "{{<a!>},{<a!>},{<a!>},{<ab>}}" => [[[]]]))

(deftest counting-test
  (are [before _ after] (= after (depths before))
    [[]] => 1
    [[[]]] => 3
    [[[[]]]] => 6
    [[[] []]] => 5
    [[[] [] [] []]] => 9
    [[[[] [] [[]]]]] => 16))

(deftest count-garbage-test
  (are [before _ after] (= after (count-garbage before))
    "<>" => 0
    "<random characters>" => 17
    "<<<<>" => 3
    "<{!>}>" => 2
    "<!!>" => 0
    "<!!!>>" => 0
    "<{o\"i!a,<{i<a>" => 10))

(run-all-tests #"day7.*")
