(ns fsms.programs.while)

(def MAX-STEPS 10000)

(defn analyse-instr [prog ids]
  (case (first prog)
    :AssignConstant (when (ids (second prog)) {:error true, :msg (str "Error: Assigning loop variable " (second prog))})
    :AssignCalc     (cond (ids (second prog)) {:error true, :msg (str "Error: Assigning loop variable " (second prog))}
                          (ids (nth prog 2)) {:error true, :msg (str "Error: Reading loop variable " (nth prog 2))})
    :While (if (ids (second prog))
            {:error true, :msg (str "Error: Reading loop variable " (second prog))}
            (let [ress (keep #(analyse-instr % ids) (drop 2 prog))]
              (first ress)))  
    :Loop (if (ids (second prog))
            {:error true, :msg (str "Error: Reading loop variable " (second prog))}
            (let [ress (keep #(analyse-instr % (conj ids (second prog))) (drop 2 prog))]
              (first ress)))))

(defn analyse 
  ([loop-program] (analyse loop-program #{}))
  ([loop-program locked-ids] 
   (first (keep #(analyse-instr % locked-ids) loop-program))))


(defn interp-step [instrs env]
  (if (empty? instrs)
    {:program nil, :env env}
    (case (ffirst instrs)
      :AssignConstant (let [[_assignconst id const] (first instrs)]
                        {:program (rest instrs)
                         :env (assoc env id (parse-long const))})
      :AssignCalc (let [[_assigncalc id-lhs id-rhs op const] (first instrs)
                        arg1 (get env id-rhs 0)
                        arg2 (parse-long const)
                        res (max ((case op "+" + "-" -) arg1 arg2) 0)]
                    {:program (rest instrs)
                     :env (assoc env id-lhs res)})
      :While (let [[_while id-while & body :as instr] (first instrs)]
               (if (= 0 (get env id-while 0))
                 {:program (rest instrs)
                  :env env}
                 {:program (concat body [instr] (rest instrs))
                  :env env}))
      :Loop (let [[_loop id-loop & body] (first instrs)]
              {:program (concat (apply concat (repeat (get env id-loop 0) body)) (rest instrs))
               :env env}))))

(defn interp [program env]
  (loop [step 0
         seen #{}
         {:keys [program env]} {:program program :env env}]
    (cond (not program) env
          (>= step MAX-STEPS) :timeout
          (seen [program env]) :timeout ;; we are even sure it is an infinite loop
          :otherwise (recur (inc step) (conj seen [program env]) (interp-step program env)))))

(defn print-program [c]
  (doseq [l c] (println l)))


(comment
(use 'programs.parser)
  
(interp [[:While "x0" [:AssignConstant "x1" "1"] [:AssignConstant "x2" "2"]]
              [:AssignConstant "x3" "3"]]
             {"x0" 1}
             )
(analyse (parse-loop-program
           "x0 := x1 + 0;
           LOOP x2 DO
           x2 := x1 + 1
           END"
           ))

(interp 
  (parse-loop-program
    "x0 := x1 + 0;
    LOOP x2 DO
    x0 := x0 + 1
    END"
    )
  {"x1" 5
   "x2" 4
   }
  )

(interp 
  (parse-while-program
    "x0 := x1 + 0;
    LOOP x2 DO
    x0 := x0 + 1
    END"
    )
  {"x1" 5
   "x2" 4
   }
  )

(require 'instaparse.failure)
(analyse (with-out-str (instaparse.failure/pprint-failure (parse-loop-program (slurp "resources/add.while")))))
(interp
(parse-while-program 
  "x1 := x2 + 0;
  WHILE x1 /= 0 DO x1 := x1 - 1; x42 := x42 + 1 END")
{"x1" 5
 "x2" 9 
 "x42" 8 
 }
    
  )

(interp-step
  (parse-loop-program
    "LOOP x2 DO
    x0 := x0 + 1
    END"
    )
  {"x1" 5
   "x2" 4
   }
  )

(analyse (parse-loop-program
           "x0 := x2 + 0"
           )
         #{"x2"}
         )

  )
