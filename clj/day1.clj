(ns user (:require [clojure.string :as str]))

(println
 (->> (slurp "input/day1.txt")
      str/split-lines
      (partition-by str/blank?)
      (map #(reduce (fn [a x] (+ a (or (parse-long x) 0))) 0 %))
      (apply max))

 (->> (slurp "input/day1.txt")
      str/split-lines
      (partition-by str/blank?)
      (map #(reduce (fn [a x] (+ a (or (parse-long x) 0))) 0 %))
      sort
      (take-last 3)
      (apply +)))
