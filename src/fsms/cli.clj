(ns fsms.cli
  (:require [clojure.string]
            [clojure.tools.cli :refer [parse-opts]]))

(def valid-cli-options
  "java -jar mytool.jar COMMAND [ARGS*]

Valid options for COMMAND are:
  check-regex <regex-file> <config-file>
   | Check a regex against a number of accepted
   | and rejected words.
  check-nfa <nfa-file> <config-file>
   | Check an implementation of a NFA against
   | a number of accepted and rejected words.
  check-dfa <dfa-file> <config-file>
   | Like check-nfa, but input automata
   | must be deterministic
  check-dpda <dpda-file> <config-file>
   | Check an implementation of a DPDA against
   | a number of accepted and rejected words.
  check-tm <tm-file> <config-file>
   | Check an implementation of a TM against
   | a number of accepted and rejected words.  
  check-dtm <tm-file> <config-file>
   | Like check-tm, but also ensure TM is deterministic.
  check-lba <tm-file> <config-file>
   | Like check-lba, but assumes TM is an LBA.
  check-calc-dtm <tm-file> <config-file>
   | Check an implementation of a deterministic TM used for calculation.
   | Verify that input-output pairs are satisfied.
  check-loop-program <loop-file> <config-file>
   | Like check-calc-dtm, but input is a LOOP-program.
  check-while-program <while-file> <config-file>
   | Like check-calc-dtm, but input is a WHILE-program.
  check-goto-program <goto-file> <config-file>
   | Like check-calc-dtm, but input is a GOTO-program.")

(defn usage [summary]
  (str valid-cli-options \newline \newline
       "Valid options for ARGS are:" \newline
       summary))

(def cli-options
  [[nil "--debug" "Be more (or very verbose) in the output."]
   ["-f" "--output FILE" "File to output results into."
    :default nil]
   ["-s" "--score SCORE" "Maximum score for the exercise."
    :default 10
    :parse-fn parse-long]
   ["-h" "--help"]])

(defn validate-args [args]
  (let [{:keys [options arguments errors summary]} (parse-opts args cli-options)]
    (cond
      (:help options) ; help => exit OK with usage summary
      {:exit-message (usage summary) :ok? true}
      errors ; errors => exit with description of errors
      {:exit-message (clojure.string/join errors)}
      ;; custom validation on arguments
      (and (contains? #{"check-regex" "check-dfa" "check-nfa" "check-dpda" "check-tm" "check-dtm" "check-lba"
                        "check-calc-dtm"
                        "check-loop-program" "check-while-program" "check-goto-program"}
                      (first arguments))
           (= 3 (count arguments)))
      {:action (first arguments) :args (rest arguments)
       :options options}

      :else ; failed custom validation => exit with usage summary
      {:exit-message (usage summary)})))

(defn exit [status msg]
  (println msg)
  (System/exit status))
