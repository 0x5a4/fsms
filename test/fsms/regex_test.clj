(ns fsms.regex-test
  (:use [fsms.regex]
        [clojure.test]))

(defn- check-match [regex word]
  (re-matches (build-regex regex) word))

(deftest regex-matches
  (testing "regex single symbols"
    (are [regex match] (check-match regex match)
      "a"
      "a"

      "7"
      "7"))

  (testing "regex lambda matches"
    (is (check-match "_" ""))
    (is (not (check-match "_" "7"))))

  (testing "regex kleene star"
    (is (check-match "(_)*" ""))
    (are [match] (check-match "(a)*" match)
      ""
      "a"
      "aaaa"
      "aaaa"))

  (testing "regex or"
    (are [match] (check-match "(a + b)" match)
      "a"
      "b"))

  (testing "regex concat"
    (is (check-match "abc" "abc"))
    (is (not (check-match "bc" "abc")))))
