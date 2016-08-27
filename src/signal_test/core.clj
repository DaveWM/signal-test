(ns signal-test.core
  (:require [clojure.string :refer [join]]))

(def digit-strings {1 "one"
                    2 "two"
                    3 "three"
                    4 "four"
                    5 "five"
                    6 "six"
                    7 "seven"
                    8 "eight"
                    9 "nine"})

(def tens-strings {2 "twenty"
                   3 "thirty"
                   4 "forty"
                   5 "fifty"
                   6 "sixty"
                   7 "seventy"
                   8 "eighty"
                   9 "ninety"})

(def teens-strings {0 "ten"
                    1 "eleven"
                    2 "twelve"
                    3 "thirteen"
                    4 "fourteen"
                    5 "fifteen"
                    6 "sixteen"
                    7 "seventeen"
                    8 "eighteen"
                    9 "nineteen"})

(def units [nil "thousand" "million" "billion" "trillion" "quadrillion"])

(defn pow [x n]
  "returns x^n"
  (apply * (repeat n x)))

(defn split-number [n group-size]
  "Splits a number into groups, with the given length. For example, (split-number 123456 3) returns (456 123). Returns an infinite seq"
  (let [group-multiplier (pow 10 group-size)]
    (map #(-> (quot n %)
              (mod group-multiplier))
         (iterate (partial * group-multiplier) 1))))

(defn group-to-vec [n]
  "Returns a vector representing a group of 3 or fewer numbers"
  (let [[singles tens hundreds] (split-number n 1)]
    [(when (not (zero? hundreds))
       (str (get digit-strings hundreds) " hundred"))
     (when tens (get tens-strings tens))
     (let [numbers-map (if (= 1 tens) teens-strings digit-strings)]
       (get numbers-map singles))]))

(defn group-vec-to-str [group]
  "Converts the vector representation of a group into a string"
  (let [[hundreds tens singles] group
        add-and (and hundreds (or tens singles))]
    (->> [hundreds (when add-and "and") tens singles]
         (remove nil?)
         (join " "))))

(defn number-to-string [n]
  "Converts a positive integer into an english sentence"
  (let [groups (split-number n 3)]
    (->> (map
          (fn [group unit] [(group-to-vec group) unit])
          groups units)
         (filter (fn [[group-vec unit]] (some (complement nil?) group-vec)))
         (map (fn [[group-vec unit]] (str (group-vec-to-str group-vec) (when unit (str " " unit)))))
         reverse
         (join ", "))))


