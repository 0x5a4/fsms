(ns fsms.while-test
  (:use [fsms.programs.while]
        [clojure.test])
  (:require [clojure.java.io :as io]
            [fsms.core :as core]))

(deftest while-parse-test
  (testing "while program gets parsed correctly"
    (are [program tree] (= tree (parse-while-program program))
      ; AssignCalc
      "x0 := x1 + 1"
      [[:AssignCalc "x0" "x1" "+" "1"]]
      "x0 := x1 - 1"
      [[:AssignCalc "x0" "x1" "-" "1"]]

      ; AssignConstant
      "x0 := 1"
      [[:AssignConstant "x0" "1"]]

      ; Loop, single instruction
      "LOOP x1 DO x0 := x0 + 1 END"
      [[:Loop "x1" [:AssignCalc "x0" "x0" "+" "1"]]]

      ; Loop, multiple instructions
      "LOOP x1 DO x0 := x0 + 1; x2 := 5 END"
      [[:Loop "x1" [:AssignCalc "x0" "x0" "+" "1"]
        [:AssignConstant "x2" "5"]]]

      ; While, single instruction
      "WHILE x1 /= 0 DO x0 := x0 + 1 END"
      [[:While "x1" [:AssignCalc "x0" "x0" "+" "1"]]]

      ; While, multiple instructions
      "WHILE x1 /= 0 DO x0 := x0 + 1; x2 := 5 END"
      [[:While "x1" [:AssignCalc "x0" "x0" "+" "1"]
        [:AssignConstant "x2" "5"]]])))

(deftest loop-parse-test
  (testing "loop program gets parsed correctly"
    (are [program tree] (= tree (parse-loop-program program))
      ; AssignCalc
      "x0 := x1 + 1"
      [[:AssignCalc "x0" "x1" "+" "1"]]
      "x0 := x1 - 1"
      [[:AssignCalc "x0" "x1" "-" "1"]]

      ; AssignConstant
      "x0 := 1"
      [[:AssignConstant "x0" "1"]]

      ; Loop, single instruction
      "LOOP x1 DO x0 := x0 + 1 END"
      [[:Loop "x1" [:AssignCalc "x0" "x0" "+" "1"]]]

      ; Loop, multiple instructions
      "LOOP x1 DO x0 := x0 + 1; x2 := 5 END"
      [[:Loop "x1" [:AssignCalc "x0" "x0" "+" "1"]
        [:AssignConstant "x2" "5"]]])))

(deftest while-analyze-test
  (testing "correct program doesnt raise any errors"
    (is (not (:error (analyse (parse-while-program "WHILE x0 /= 0 DO x1 := x1 + 1 END") {"x2" 5})))))

  (testing "locked vars cannot be accessed"
    (are [program locked-ids] (:error (analyse (parse-while-program program) locked-ids))
       ; AssignConstant
      "WHILE x0 /= 0 DO x1 := 1 END"
      {"x1" 5}

       ; AssignCalc write
      "WHILE x0 /= 0 DO x1 := x2 + 1 END"
      {"x1" 5}

       ; AssignCalc operand
      "WHILE x0 /= 0 DO x2 := x1 + 1 END"
      {"x1" 5}

       ; While condition
      "WHILE x1 /= 0 DO x2 := x3 + 1 END"
      {"x1" 5}

       ; Loop condition
      "LOOP x1 DO x2 := x3 + 1 END"
      {"x1" 5})))

(deftest while-interp-test
  (testing "timeout works correctly"
    (are [program env] (= :timeout (interp (parse-while-program program) env))
      ; Max Steps exceeded
      "WHILE x0 /= 0 DO x0 := x0 - 1 END"
      {"x0" 10001}

      ; Infinite Loop detected
      "WHILE x0 /= 0 DO x1 := x0 - 1 END"
      {"x0" 10001}))

  (testing "calculations work as expected"
    (are [program env result] (= result (interp (parse-while-program program) env))
      ; assign constant
      "x0 := 1"
      {}
      {"x0" 1}

      ; assign calculation add
      "x0 := x0 + 1"
      {"x0" 1}
      {"x0" 2}

      ; assign calculation subtract
      "x0 := x0 - 1"
      {"x0" 1}
      {"x0" 0}

      ; while
      "WHILE x0 /= 0 DO x1 := x1 + 1; x0 := x0 - 1 END"
      {"x0" 5}
      {"x0" 0 "x1" 5}

      ; loop
      "LOOP x0 DO x1 := x1 + 1 END"
      {"x0" 5}
      {"x0" 5 "x1" 5})))
