(ns fsms.programs.goto
  (:require [instaparse.core :as insta]))

(def MAX-STEPS 10000)

(defn gather-markings [instrs]
  (into {} (keep-indexed (fn [idx instr]
                           (when (= :Label1 (first (second instr)))
                             [(second (second instr)) idx]))
                         instrs)))

(defn interp-step [program marking env]
  (let [instr (get program (get env ::pc))
        instr (if (= :Label1 (first (second instr))) (get instr 2) (second instr))]
    (case (first instr)
      nil {:error (str "No more instructions found. You are probably missing a HALT-instruction.")}
      :AssignConstant {:env (-> env
                                (assoc (second instr) (parse-long (get instr 2)))
                                (update ::pc inc))}
      :AssignCalc {:env (-> env
                            (assoc (second instr) (max 0 ((case (get instr 3) "+" + "-" -)
                                                          (get env (get instr 2) 0)
                                                          (parse-long (get instr 4)))))
                            (update ::pc inc))}
      :Goto (if (marking (second instr))
              {:env (assoc env ::pc (get marking (second instr)))}
              {:error (str "Unknown label: " (second instr))})
      :Jump (if (= 0 (get env (second instr)) 0)
              (if (marking (get instr 2))
                {:env (assoc env ::pc (get marking (get instr 2)))}
                {:error (str "Unknown label: " (get instr 2))})
              {:env (update env ::pc inc)})
      :Halt {:terminated true, :env env})))

(defn interp [program env]
  (let [program (vec program)
        markings (gather-markings program)]
    (loop [step 0
           status {:env (assoc env ::pc 0)}]
      (if (>= step MAX-STEPS)
        :timeout
        (let [status' (interp-step program markings (get status :env))]
          (cond
            (:error status') (:error status')
            (:terminated status') (get status' :env)
            :else (recur (inc step) status')))))))

(def goto-parser
  (insta/parser
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
   <WS> := <#'\\s*'>"))

(defn parse-goto-program [s]
  (insta/transform
   {:Id (partial apply str)
    :Label (partial apply str)}
   (goto-parser s)))
