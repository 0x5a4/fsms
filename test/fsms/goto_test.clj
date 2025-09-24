(ns fsms.goto-test
  (:use [fsms.programs.goto]
        [clojure.test])
  (:require [clojure.java.io :as io]
            [fsms.core :as core]))

(deftest goto-parse-test
  (testing "goto program gets parsed correctly"
    (are [program tree] (= tree (parse-goto-program program))
      ; AssignCalc
      "x0 := x1 + 1;"
      [[:Instr [:AssignCalc "x0" "x1" "+" "1"]]]
      "x0 := x1 - 1;"
      [[:Instr [:AssignCalc "x0" "x1" "-" "1"]]]

      ; AssignConstant
      "x0 := 1;"
      [[:Instr [:AssignConstant "x0" "1"]]]

      ; Goto
      "GOTO MyLabel;"
      [[:Instr [:Goto "MyLabel"]]]

      ; HALT
      "HALT;"
      [[:Instr [:Halt]]]

      ; JUMP
      "IF x0 = 0 THEN GOTO MyLabel;"
      [[:Instr [:Jump "x0" "MyLabel"]]]

      ;; Multiple Instructions
      "x0 := 1; x1 := 1;"
      [[:Instr [:AssignConstant "x0" "1"]] [:Instr [:AssignConstant "x1" "1"]]]

      ; Label
      "MyLabel: x0 := 1;"
      [[:Instr [:Label1 "MyLabel"] [:AssignConstant "x0" "1"]]])))

(deftest goto-gather-markings
  (testing "labels are gathered correctly"
    (is (= (gather-markings (parse-goto-program "x0 := 1;
                                                Hallo: x1:= 2;
                                                x0 := 1;
                                                Tschuss: x1:= 2;
                                                "))
           {"Hallo" 1 "Tschuss" 3}))))

(deftest goto-interp-test
  (testing "timeout works correctly"
    (are [program env] (= :timeout (interp (parse-goto-program program) env))
      ; Max Steps exceeded
      "Loop: x0 := 0;
       IF x0 = 0 THEN GOTO Loop;"
      {"x0" 0}))

  (testing "unknown labels raise an error"
    (are [program] (string? (interp (parse-goto-program program) {}))
      "GOTO Loop;"
      "IF x0 = 0 THEN GOTO Loop;"))

  (testing "calculations work as expected"
    (are [program env result] (= result (interp (parse-goto-program program) env))
      ; assign constant
      "x0 := 1; HALT;"
      {}
      {"x0" 1 :fsms.programs.goto/pc 1}

      ; assign calculation add
      "x0 := x0 + 1; HALT;"
      {"x0" 1}
      {"x0" 2 :fsms.programs.goto/pc 1}

      ; assign calculation subtract
      "x0 := x0 - 1; HALT;"
      {"x0" 1}
      {"x0" 0 :fsms.programs.goto/pc 1}

      ; Goto
      "GOTO Label;
       x0 := 5;
       HALT;
       Label: x0 := 1;
       HALT;"
      {}
      {"x0" 1 :fsms.programs.goto/pc 4}

      ; Conditional jump - taking the branch
      "IF x0 = 0 THEN GOTO Label;
       x0 := 5;
       HALT;
       Label: x0 := 1;
       HALT;"
      {"x0" 0}
      {"x0" 1 :fsms.programs.goto/pc 4}

      ; Conditional jump - not taking the branch
      "IF x0 = 0 THEN GOTO Label;
       x0 := 5;
       HALT;
       Label: x0 := 1;
       HALT;"
      {"x0" 5}
      {"x0" 5 :fsms.programs.goto/pc 2})))
