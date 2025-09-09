(ns fsms.programs.goto)

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


(comment
(use 'programs.parser)

(interp (parse-goto-program
                   "M1 : x0 := x1 + 0;
                   M2 : IF x2 = 0 THEN GOTO M6;
                   x0 := 42;
                   x2 := x2 - 1;
                   M5 : GOTO M2;
                   M6 : x0 := x0 + 1;
                   "
                   )
        {"x1" 5 "x2" 4 ::pc 5}
        )



(gather-markings (parse-goto-program
                   "M1 : x0 := x1 + 0;
                   M2 : IF x2 = 0 THEN GOTO M6;
                   x0 := x0 + 1;
                   x2 := 1;
                   M5 : GOTO M2;
                   M6 : HALT;"
                   )))

