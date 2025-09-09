(ns fsms.pda-test
  (:use [fsms.automata.pda]
        [clojure.test]))

(deftest initial-configurations-test
  (testing "the initial configuration (singular) of the DPDA is correct"
    (are [x y z] (= x (initial-configurations y z))
      [{:state "z0" :input "foo" :stack "#"}]
      {:start "z0"} "foo"
      [{:state "z_0" :input "foo" :stack "#"}]
      {:start "z_0"} "foo"
      [{:state "z_0" :input "a" :stack "#"}]
      {:start "z_0"} "a"
      [{:state "z_0" :input "" :stack "#"}]
      {:start "z_0"} "")))

(deftest next-states-test
  (testing "the successor configurations are calculated correctly"
    (are [pda config succs] (= (set succs) (set (next-states pda config)))
         ;; wrong state
      {:delta {{:state "z1", :symbol "0", :top-of-stack "#"} [{:state "z1", :new-stack "A#"}]}}
      {:state "z0", :input "000", :stack "#"}
      #_=> []

         ;; wrong symbol
      {:delta {{:state "z0", :symbol "1", :top-of-stack "#"} [{:state "z1", :new-stack "A#"}]}}
      {:state "z0", :input "000", :stack "#"}
      #_=> []

         ;; correct state, pushing a symbol
      {:delta {{:state "z0", :symbol "0", :top-of-stack "#"} [{:state "z1", :new-stack "A#"}]}}
      {:state "z0", :input "000", :stack "#"}
      #_=> [{:state "z1", :input "00", :stack "A#"}]

         ;; correct state, replacing a symbol
      {:delta {{:state "z0", :symbol "0", :top-of-stack "A"} [{:state "z1", :new-stack ""}]}}
      {:state "z0", :input "000", :stack "A#"}
      #_=> [{:state "z1", :input "00", :stack "#"}]

         ;; correct state, replacing last symbol
      {:delta {{:state "z0", :symbol "0", :top-of-stack "#"} [{:state "z1", :new-stack ""}]}}
      {:state "z0", :input "000", :stack "#"}
      #_=> [{:state "z1", :input "00", :stack ""}]

         ;; lambda transition
      {:delta {{:state "z0", :symbol "_", :top-of-stack "A"} [{:state "z1", :new-stack "A"}]}}
      {:state "z0", :input "000", :stack "A#"}
      #_=> [{:state "z1", :input "000", :stack "A#"}]

         ;; lambda transition
      {:delta {{:state "z0", :symbol "_", :top-of-stack "#"} [{:state "z1", :new-stack ""}]}}
      {:state "z0", :input "000", :stack "#"}
      #_=> [{:state "z1", :input "000", :stack ""}]

         ;; two transitions applicable -- can't be, only after refactoring
      )))

(deftest dpda-accepting-test
  (testing "configurations are accepted"
    (is (dpda-accepting-configuration? {:final-states #{"z0"}} {:state "z0", :input "", :stack ""}))
    (is (dpda-accepting-configuration? {:final-states #{"z0"}} {:state "z0", :input "", :stack "#"}))
    (is (dpda-accepting-configuration? {:final-states #{"z0"}} {:state "z0", :input "", :stack "ABC"}))
    (is (dpda-accepting-configuration? {:final-states #{"z1"}} {:state "z1", :input "", :stack "ABC"}))))

(deftest not-accepting-test
  (testing "configurations are not accepted"
    (is (not (dpda-accepting-configuration? {:final-states #{"z0"}} {:state "z1", :input "", :stack ""})))
    (is (not (dpda-accepting-configuration? {:final-states #{"z0"}} {:state "z1", :input "", :stack "#"})))
    (is (not (dpda-accepting-configuration? {:final-states #{"z0"}} {:state "z0", :input "a", :stack ""})))
    (is (not (dpda-accepting-configuration? {:final-states #{"z0"}} {:state "z0", :input "a", :stack "#"})))))

(deftest pda-parse-test
  (testing "pda gets parsed correctly"
    (are [program tree] (= tree (pda-parser program))
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

      ;; multiple final states
      "final z0 z1"
      [[:FINAL "z0" "z1"]]

      ;; transition
      "(z0, a, #) -> (z2, A)"
      [[:TRANS "z0" "a" "#" "z2" "A"]]

      ;; complete example
      "start z0
       final z1
       (z0, a, A) -> (z1, #)
       (z1, a, B) -> (z2, Z)"
      [[:START "z0"]
       [:FINAL "z1"]
       [:TRANS "z0" "a" "A" "z1" "#"]
       [:TRANS "z1" "a" "B" "z2" "Z"]])))

(deftest pda-build-test
  (testing "nfa gets built correctly"
    (are [program nfa] (= nfa (build-pda (pda-parser program)))
      ;; start state
      "start z0"
      {:start "z0" :final-states [] :delta {}}

      ;; final state
      "final z0"
      {:start nil :final-states ["z0"] :delta {}}

      ;; single transition
      "(z0, a, A) -> (z2, B)"
      {:start nil
       :final-states []
       :delta {{:state "z0" :symbol "a" :top-of-stack "A"} [{:state "z2" :new-stack "B"}]}}

      ;; non-deterministic transitions
      "(z0, a, #) -> (z2, A)
       (z0, a, #) -> (z5, B)"
      {:start nil
       :final-states []
       :delta {{:state "z0" :symbol "a" :top-of-stack "#"} [{:state "z2" :new-stack "A"}
                                                            {:state "z5" :new-stack "B"}]}}

      ;; complete example
      "start z0
       final z1
       (z0, a, A) -> (z1, #)
       (z1, a, B) -> (z2, Z)"
      {:start "z0"
       :final-states ["z1"]
       :delta {{:state "z0" :symbol "a" :top-of-stack "A"} [{:state "z1" :new-stack "#"}]
               {:state "z1" :symbol "a" :top-of-stack "B"} [{:state "z2" :new-stack "Z"}]}})))

(deftest pda-validate-deterministic
  (testing "deterministic pdas get recognized as such"
    (are [pda] (nil? (validate-deterministic pda))
      {:delta {{:state "z0" :symbol "0" :top-of-stack "#"} [{:state "z0" :new-stack "A#"}]
               {:state "z0" :symbol "_" :top-of-stack "A"} [{:state "z0" :new-stack "AA"}]
               {:state "z0" :symbol "1" :top-of-stack "#"} [{:state "z1" :new-stack "#"}]
               {:state "z1" :symbol "1" :top-of-stack "#"} [{:state "z1" :new-stack "#"}]}}))

  (testing "non-deterministic pdas get recognized as such"
    (are [pda] (thrown? AssertionError (validate-deterministic pda))
      {:delta {{:state "z0" :symbol "a" :top-of-stack "A"} [{:state "z0" :new-stack "A"} {:state "z1" :new-stack "B"}]}}

      {:delta {{:state "z0" :symbol "_" :top-of-stack "A"} []
               {:state "z0" :symbol "a" :top-of-stack "A"} []}})))
