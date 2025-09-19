(ns fsms.regex-test
  (:use [fsms.regex]
        [clojure.test])
  (:require
   [instaparse.core :as insta]))

(defn- check-match [regex word]
  (re-matches (build-regex (regex-parser regex)) word))

(deftest regex-emptyset-folding
  (testing "emptyset is folded in or nodes"
    (are [regex nodes] (= nodes (first (insta/transform fold-emptyset (regex-parser regex))))
      ; doesnt contain an emptyset and is left as-is 
      "(a + b)"
      [:OR [:SYM "a"] [:SYM "b"]]

      ; emptyset on lhs is folded away
      "({} + a)"
      [:SYM "a"]

      ; emptyset on rhs is folded away
      "(a + {})"
      [:SYM "a"]

      ; emptyset on both sides is collapsed to emptyset
      "({} + {})"
      [:EMPTYSET]))

  (testing "emptyset is folded in concat nodes"
    (are [regex nodes] (= nodes (first (insta/transform fold-emptyset (regex-parser regex))))
      ; doesnt contain an emptyset and is left as-is 
      "ab"
      [:CONCAT [:SYM "a"] [:SYM "b"]]

      ; emptyset on lhs is folded away
      "{}a"
      [:SYM "a"]

      ; emptyset on rhs is folded away
      "a{}"
      [:SYM "a"]

      ; emptyset on both sides is collapsed to emptyset
      "{}{}"
      [:EMPTYSET]))

  (testing "emptyset is transformed in kleene nodes"
    (are [regex nodes] (= nodes (first (insta/transform fold-emptyset (regex-parser regex))))
      ; doesnt contain an emptyset and is left as-is 
      "(a)*"
      [:KLEENE [:SYM "a"]]

      ; emptyset is folded to lambda
      "({})*"
      [:LAMBDA])))

(deftest regex-build
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

(deftest regex-accept-fn
  (testing "regex accept-fn accepts when it should"
    (is (accept? #"a" "a"))
    (is (accept? #"(a|b)" "b")))
  
  (testing "regex accept-fn rejects when it should"
    (is (not (accept? #"b" "a")))
    (is (not (accept? #"(a|b)" "c"))
    (is (not (accept? :EMPTYSET "b"))))))
