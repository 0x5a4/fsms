(ns fsms.turing-test
  (:use [fsms.automata.turing-machine]
        [clojure.test]))

(deftest tm-initial-configurations-test
  (testing "the initial configurations of a TM are correct"
    (are [x y z] (= x (initial-configurations y z))
      [{:state "z0" :band-left blank :current "f" :band-right "oo"}]
      {:start "z0"} "foo"

      [{:state "z0" :band-left blank :current "f" :band-right blank}]
      {:start "z0"} "f")))

(deftest lba-initial-configurations-test
  (testing "the initial configurations of a LBA are correct"
    (are [x y z] (= x (initial-lba-configurations y z))
      [{:state "z0" :band-left "" :current "f" :band-right "oO"}]
      {:start "z0" :symbols {"o" "O"}} "foo")))

(deftest tm-step-test
  (testing "the successor configurations of a TM are calculated correctly"
    (are [tm config succs] (= (set succs) (set (turing-step tm config)))
      ;; wrong state
      {:delta {{:state "z0" :symbol "0"} {}}}
      {:state "z1" :band-left "12" :current "0" :band-right "45"}
      []

      ;; wrong symbol
      {:delta {{:state "z0" :symbol "0"} {}}}
      {:state "z0" :band-left "12" :current "3" :band-right "45"}
      []

      ;; Neutral Transition
      {:delta {{:state "z0" :symbol "3"} [{:state "z0" :symbol "A" :direction "N"}]}}
      {:state "z0" :band-left "12" :current "3" :band-right "45"}
      [{:state "z0" :band-left "12" :current "A" :band-right "45"}]

      ;; Left Transition
      {:delta {{:state "z0" :symbol "3"} [{:state "z0" :symbol "A" :direction "L"}]}}
      {:state "z0" :band-left "12" :current "3" :band-right "45"}
      [{:state "z0" :band-left "1" :current "2" :band-right "A45"}]

      ;; Right Transition
      {:delta {{:state "z0" :symbol "3"} [{:state "z0" :symbol "A" :direction "R"}]}}
      {:state "z0" :band-left "12" :current "3" :band-right "45"}
      [{:state "z0" :band-left "12A" :current "4" :band-right "5"}])))

(deftest lba-step-test
  (testing "the successor configuration of a LBA are calculated correctly"
    (are [tm config succs] (= (set succs) (set (lba-step tm config)))
      ;; wrong state
      {:delta {{:state "z0" :symbol 0} {}}}
      {:state "z1" :band-left "12" :current "0" :band-right "45"}
      []

      ;; wrong symbol
      {:delta {{:state "z0" :symbol 0} {}}}
      {:state "z1" :band-left "12" :current "A" :band-right "45"}
      []

      ;; Neutral Transition
      {:delta {{:state "z0" :symbol "3"} [{:state "z0" :symbol "A" :direction "N"}]}}
      {:state "z0" :band-left "12" :current "3" :band-right "45"}
      [{:state "z0" :band-left "12" :current "A" :band-right "45"}]

      ;; Left Transition
      {:delta {{:state "z0" :symbol "3"} [{:state "z0" :symbol "A" :direction "L"}]}}
      {:state "z0" :band-left "12" :current "3" :band-right "45"}
      [{:state "z0" :band-left "1" :current "2" :band-right "A45"}]

      ;; Right Transition
      {:delta {{:state "z0" :symbol "3"} [{:state "z0" :symbol "A" :direction "R"}]}}
      {:state "z0" :band-left "12" :current "3" :band-right "45"}
      [{:state "z0" :band-left "12A" :current "4" :band-right "5"}])))

(deftest turing-accept-test
  (testing "TM configurations are accepted or rejected accordingly"
    ;; i am valid
    (is true? (turing-accepting? {:final-states #{"z1" "z2"}} {:state "z1"}))
    ;; i am not
    (is false? (turing-accepting? {:final-states #{"z1" "z2"}} {:state "z3"}))))

(deftest turing-parse-test
  (testing "TM gets parsed correctly"
    (are [program tree] (= tree (tm-parser program))
      ;; start state
      "start z0"
      [[:START "z0"]]

      ;; final state
      "final z0"
      [[:FINAL "z0"]]

      ;; comments
      "; aaaaa   7 6 a    
       ;
       start z0      ; aaaaaaaa"
      [[:START "z0"]]

      ;; transition
      "(z0, a) -> (z2, a, L)"
      [[:TRANS "z0" "a" "z2" "a" "L"]]

      ;; complete example
      "start z0
       final z1
       (z0, a) -> (z2, a, L)
       (z1, b) -> (z2, B, N)"
      [[:START "z0"]
       [:FINAL "z1"]
       [:TRANS "z0" "a" "z2" "a" "L"]
       [:TRANS "z1" "b" "z2" "B" "N"]])))

(deftest turing-build-test
  (testing "nfa gets built correctly"
    (are [program nfa] (= nfa (build-tm (tm-parser program)))
      ;; start state
      "start z0"
      {:start "z0" :final-states #{} :symbols {"_" "_"} :delta {}}

      ;; final state
      "final z0"
      {:start nil :final-states #{"z0"} :symbols {"_" "_"} :delta {}}

      ;; single transition
      "(z0, a) -> (z2, a, L)"
      {:start nil :final-states #{} :symbols {"_" "_"} :delta {{:state "z0" :symbol "a"} [{:state "z2" :symbol "a" :direction "L"}]}}

      ;; non-deterministic transitions
      "(z0, a) -> (z2, a, L)
       (z0, a) -> (z5, a, R)"
      {:start nil
       :final-states #{}
       :symbols {"_" "_"}
       :delta {{:state "z0" :symbol "a"} [{:state "z2" :symbol "a" :direction "L"}
                                          {:state "z5" :symbol "a" :direction "R"}]}}
      ;; complete example
      "start z0
       final z1
       (z0, a) -> (z2, a, L)
       (z0, a) -> (z5, a, R)
       (z1, b) -> (z2, B, N)"
      {:start "z0"
       :final-states #{"z1"}
       :symbols {"_" "_"}
       :delta {{:state "z0" :symbol "a"} [{:state "z2" :symbol "a" :direction "L"}
                                          {:state "z5" :symbol "a" :direction "R"}]
               {:state "z1" :symbol "b"} [{:state "z2" :symbol "B" :direction "N"}]}})))
