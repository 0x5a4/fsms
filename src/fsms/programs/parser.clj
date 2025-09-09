(ns fsms.programs.parser
  (:require [instaparse.core :as insta]))

(def loop-grammar
  "<Instrs> := Instr <';'> Instrs | Instr
   <Instr> := AssignConstant | AssignCalc | Loop
   AssignCalc := Id <':='> Id Op Number 
   AssignConstant := Id <':='> Number
   Loop := WS <'LOOP'> Id <'DO'> Instrs <'END'> WS
   <Op> := '+' | '-'
   Id := WS 'x' Number WS
   <Number> := WS #'[0-9]+' WS
   <WS> := <#'\\s*'>")

(def loop-parser (insta/parser loop-grammar))

(defn parse-loop-program [s]
  (insta/transform
    {:Id (partial apply str)}
    (loop-parser s)))


(def while-grammar
  "<Instrs> := Instr <';'> Instrs | Instr
   <Instr> := AssignConstant | AssignCalc | Loop | While
   AssignCalc := Id <':='> Id Op Number 
   AssignConstant := Id <':='> Number
   Loop := WS <'LOOP'> Id <'DO'> Instrs <'END'> WS
   While := WS <'WHILE'> Id <'/='> WS <'0'> WS <'DO'> Instrs <'END'> WS
   <Op> := '+' | '-'
   Id := WS 'x' Number WS
   <Number> := WS #'[0-9]+' WS
   <WS> := <#'\\s*'>"
  )

(def while-parser (insta/parser while-grammar))

(defn parse-while-program [s]
  (insta/transform
    {:Id (partial apply str)}
    (while-parser s)))

(def goto-grammar
  "<Program> := Instr+
   Instr := WS Label1? Instr2 WS <';'> WS
   Label1 := Label WS <':'> WS
   Label := #'\\w'+
   <Instr2> := AssignConstant | AssignCalc | Goto | Jump | Halt
   AssignCalc := Id <':='> Id Op Number 
   AssignConstant := Id <':='> Number
   Goto := WS <'GOTO'> WS Label
   Jump := WS <'IF'> Id <'='> WS <'0'> WS <'THEN'> WS <'GOTO'> WS Label WS
   Halt := <'HALT'>
   <Op> := '+' | '-'
   Id := WS 'x' Number WS
   <Number> := WS #'[0-9]+' WS
   <WS> := <#'\\s*'> 
  "
  )

(def goto-parser (insta/parser goto-grammar))

(defn parse-goto-program [s]
  (insta/transform
    {:Id (partial apply str)
     :Label (partial apply str)
     }
    (goto-parser s)))

(defn parse-with [file f]
  (f (slurp file)))

(comment 
(parse-goto-program
  "M1 : x0 := x1 + 0;
   M2 : IF x2 = 0 THEN GOTO M6;
    x0 := x0 + 1;
   x2 := x2 - 1;
   M5 : GOTO M2;
   M6 : HALT;"
  )

(parse-loop-program 
  "x1 := 43 ; x2 := x42 + 2")

(parse-loop-program 
  "x1 := 43")

(parse-loop-program 
   "x2 := 1; x3 := 1;
   LOOP x1 DO x2 := 0 END;
   LOOP x2 DO x0 := 1; x3 := 0 END ;
   LOOP x3 DO x0 := 0 END")

(parse-loop-program
  "x0 := x1 + 0;
  LOOP x2 DO
  x0 := x0 + 1
  END"
  )

;; this does not parse as a second loop is required
(parse-loop-program
  "x0 := 0;
  LOOP x2 DO
  x0 := x0 + x1
  END"
  )

(parse-while-program 
  "x1 := x2 + 0;
  WHILE x1 /= 0 DO x1 := x1 - 1; x42 := 3 END")

)
