(ns signal-test.core-test
  (:require [clojure.test :refer :all]
            [signal-test.core :refer :all]
            [clojure.spec.test :as stest]))


(deftest number-to-string-tests
  (is (= (number-to-string 1) "one"))
  (is (= (number-to-string 123) "one hundred and twenty three"))
  (is (= (number-to-string 12) "twelve"))
  (is (= (number-to-string 44) "forty four"))
  (is (= (number-to-string 1234) "one thousand, two hundred and thirty four"))
  (is (= (number-to-string 1001) "one thousand, and one"))
  (is (= (number-to-string 1001001) "one million, one thousand, and one"))
  (is (not= nil? (stest/check `number-to-string))))
