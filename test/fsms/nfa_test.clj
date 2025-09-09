(ns fsms.nfa-test
  (:use [fsms.automata.nfa]
        [clojure.test]))

(deftest initial-configurations-test
  (testing "the initial configurations of the NFA are correct"
    (are [x y z] (= x (initial-configurations y z))
      [{:state "z0" :input "foo"}]
      {:start ["z0"]} "foo"
      [{:state "z0" :input "foo"} {:state "z1" :input "foo"}]
      {:start ["z0" "z1"]} "foo")))

(deftest next-states-test
  (testing "the successor configurations are calculated correctly"
    (are [nfa config succs] (= (set succs) (set (next-states nfa config)))
      ;; wrong state
      {:delta {{:state "z0" :symbol "0"} ["z0"]}}
      {:state "z1" :input "007"}
      []

      ;; wrong symbol
      {:delta {{:state "z0" :symbol "0"} ["z0"]}}
      {:state "z1" :input "42"}
      []

      ;; correct state and symbol
      {:delta {{:state "z0" :symbol "0"} ["z1"]}}
      {:state "z0" :input "007"}
      [{:state "z1" :input "07"}]

      ;; non-deterministic configurations
      {:delta {{:state "z0" :symbol "0"} ["z1" "z5"]}}
      {:state "z0" :input "007"}
      [{:state "z1" :input "07"} {:state "z5" :input "07"}])))

(deftest nfa-accepting-test
  (testing "configurations are accepted or rejected accordingly"
    ;; i am valid
    (is true?  (accept? {:final-states #{"z0"}} {:state "z0" :input ""}))
    ;; i am not
    (is false? (accept? {:final-states #{"z0"}} {:state "z1", :input ""}))
    (is false? (accept? {:final-states #{"z0"}} {:state "z0", :input "a"}))
    (is false? (accept? {:final-states #{"z0"}} {:state "z1", :input "a"}))))

(deftest nfa-parse-test
  (testing "nfa gets parsed correctly"
    (are [program tree] (= tree (nfa-parser program))
      ;; start state
      "start z0"
      [[:START "z0"]]

      ;; final state
      "final z0"
      [[:FINAL "z0"]]

      ;; transition
      "(z0, a) -> z2"
      [[:TRANS "z0" "a" "z2"]]

      ;; complete example
      "start z0
       final z1
       (z0, a) -> z1
       (z1, a) -> z1"
      [[:START "z0"] [:FINAL "z1"] [:TRANS "z0" "a" "z1"] [:TRANS "z1" "a" "z1"]])))

(deftest nfa-build-test
  (testing "nfa gets built correctly"
    (are [program nfa] (= nfa (build-nfa (nfa-parser program)))
      ;; start state
      "start z0"
      {:start ["z0"] :final-states [] :delta {}}

      ;; final state
      "final z0"
      {:start [] :final-states ["z0"] :delta {}}

      ;; single transition
      "(z0, a) -> z2"
      {:start [] :final-states [] :delta {{:state "z0" :symbol "a"} ["z2"]}}

      ;; non-deterministic transitions
      "(z0, a) -> z2
       (z0, a) -> z5"
      {:start [] :final-states [] :delta {{:state "z0" :symbol "a"} ["z2" "z5"]}}

      ;; complete example
      "start z0
       final z1
       (z0, a) -> z1
       (z1, a) -> z1"
      {:start ["z0"]
       :final-states ["z1"]
       :delta {{:state "z0" :symbol "a"} ["z1"]
               {:state "z1" :symbol "a"} ["z1"]}})))

(deftest nfa-validate-deterministic
  (testing "deterministic nfas get recognized as such"
    (are [nfa] (nil? (validate-deterministic nfa))
      {:start ["z1"]
       :delta {{:state "z0" :symbol "a"} ["z1"]
               {:state "z0" :symbol "b"} ["z2"]}}

      {:start ["z1"]
       :delta {{:state "z0" :symbol "a"} ["z1"]
               {:state "z1" :symbol "a"} ["z2"]}}))

  (testing "non-deterministic nfas get recognized as such"
    (are [nfa] (thrown? AssertionError (validate-deterministic nfa))
      {:start ["z1" "z2"]
       :delta {{:state "z0" :symbol "a" :to "z1"} ["z1" "z2"]}}

      {:start ["z1"]
       :delta {{:state "z0" :symbol "a" :to "z1"} ["z1" "z2"]}})))
