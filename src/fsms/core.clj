(ns fsms.core
  (:require [fsms.automata.nfa :as nfa]
            [fsms.automata.pda :as pda]
            [fsms.automata.turing-machine :as tm]
            [fsms.automata.search :refer [build-accept?-fn *debug*]]
            [fsms.programs.parser :as prog-parser]
            [fsms.programs.while :as while-progs]
            [fsms.programs.goto :as goto-progs]
            [fsms.regex :as regex]
            [fsms.cli :as cli]
            [instaparse.failure]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.string :as string])
  (:gen-class))

(defn load-config [file]
  (edn/read-string (slurp file)))

(defn validate-automaton [accept?-fn context config]
  (let [err1 (for [word (:accept config)
                   :when (not (accept?-fn context word))]
               (str "Word '" word "' should have been accepted, but was rejected"))
        err2 (for [word (:reject config)
                   :when (accept?-fn context word)]
               (str "Word '" word "' should have been rejected, but was accepted"))]
    (concat err1 err2)))

(defn validate-regex [file config]
  (let [regex (regex/file->regex file)]
    (validate-automaton regex/accept? regex config)))

(defn to-bin [n]
  (if (zero? n)
    "0"
    (loop [acc ()
           n n]
      (if (zero? n)
        (apply str acc)
        (recur (conj acc (mod n 2)) (quot n 2))))))

(defn maybe-binnify [input]
  (cond (string? input) input
        (integer? input) (to-bin input)
        (sequential? input) (string/join "#" (mapv maybe-binnify input))
        :otherwise (throw (IllegalArgumentException. (str "encountered weird input: " input)))))

(defn validate-calculations [accept?-fn automaton config result-fn]
  (for [[input output] config
        :let [input (maybe-binnify input)
              output (maybe-binnify output)
              res (result-fn (accept?-fn automaton input))]
        :when (not= res output)]
    (str "Input " input " should yield '" output "' but was '" res "' instead.")))

(defn validate-nfa [deterministic file config]
  (let [nfa (nfa/file->nfa file deterministic)
        config (load-config config)]
    (validate-automaton (build-accept?-fn nfa/initial-configurations
                                          nfa/next-states
                                          nfa/accept?
                                          nfa/discard?)
                        nfa
                        config)))

(defn validate-dpda [file config]
  (let [dpda (pda/file->pda file true)
        config (load-config config)]
    (validate-automaton (build-accept?-fn pda/initial-configurations
                                          pda/next-states
                                          pda/dpda-accepting-configuration?
                                          pda/discard-config?)
                        dpda
                        config)))

(defn validate-tm [file config]
  (let [tm (tm/file->tm file)
        config (load-config config)]
    (validate-automaton (build-accept?-fn tm/initial-configurations
                                          tm/turing-step
                                          tm/turing-accepting?
                                          tm/turing-discard?)
                        tm
                        config)))

(defn validate-lba [file config]
  (let [tm (tm/file->lba file)
        config (load-config config)]
    ;; TODO: handle exception on invalid configuration
    (validate-automaton (build-accept?-fn tm/initial-lba-configurations
                                          tm/lba-step
                                          tm/turing-accepting?
                                          tm/turing-discard?)
                        tm
                        config)))

(defn validate-dtm [file config]
  (let [tm (tm/file->tm file)
        config (load-config config)]
    (tm/assert-deterministic tm)
    (validate-automaton (build-accept?-fn tm/initial-configurations
                                          tm/turing-step
                                          tm/turing-accepting?
                                          tm/turing-discard?)
                        tm
                        config)))

(defn validate-calc-dtm [file config]
  (let [tm (tm/file->tm file)
        config (load-config config)]
    (tm/assert-deterministic tm)
    (validate-calculations (build-accept?-fn tm/initial-configurations
                                             tm/turing-step
                                             tm/turing-accepting?
                                             tm/turing-discard?)
                           tm
                           config
                           tm/result-from-configuration)))

(defn build-initial-environment [input]
  (cond (integer? input) {"x1" input}
        (vector? input) (into {} (map-indexed (fn [idx e] [(str "x" (inc idx)) e]) input))
        :else (throw (IllegalArgumentException. "unknown input type; expected integer or vector of integers, got: " input))))

(defn validate-program [program interp-fn config]
  (for [[input output] config
        :let [res (interp-fn program (build-initial-environment input))]
        :when (not (or (and (map? res) (= output (get res "x0" 0))) (= output res :timeout)))]
    (if (map? res)
      (str "Input " input " should yield '" output "' but was '" (get res "x0" 0) "' instead. Full environment: " (dissoc res :programs.goto/pc))
      (str "Error during execution with input: " input " - " res))))

(defn validate-loop-program [file config]
  (let [program (prog-parser/parse-with file prog-parser/parse-loop-program)]
    (if (instance? instaparse.gll.Failure program)
      [(string/replace (with-out-str (instaparse.failure/pprint-failure program)) "\n" "\n; ")]
      (let [config (load-config config)
            analysis-res (while-progs/analyse program)]
        (if analysis-res
          analysis-res
          (validate-program program while-progs/interp config))))))

(defn validate-while-program [file config]
  (let [program (prog-parser/parse-with file prog-parser/parse-while-program)]
    (if (instance? instaparse.gll.Failure program)
      [(string/replace (with-out-str (instaparse.failure/pprint-failure program)) "\n" "\n; ")]
      (let
       [config (load-config config)
        analysis-res (while-progs/analyse program)]
        (if analysis-res
          analysis-res
          (validate-program program while-progs/interp config))))))

(defn validate-goto-program [file config]
  (let [program (prog-parser/parse-with file prog-parser/parse-goto-program)
        config (load-config config)]
    (if (instance? instaparse.gll.Failure program)
      [(string/replace (with-out-str (instaparse.failure/pprint-failure program)) "\n" "\n; ")]
      (validate-program program goto-progs/interp config))))

(defn execute-with-output [f args opts]
  (binding [*out* (if (:output opts)
                    (io/writer (:output opts))
                    *out*)
            *debug* (if (:debug opts) true *debug*)]
    (let [res (apply f args)]
      (doseq [l res]
        (println (str "; " l)))
      (if (seq res)
        (println 0)
        (println (:score opts))))))

(defn -main [& args]
  (let [{:keys [action args options exit-message ok?]} (cli/validate-args args)]
    (if exit-message
      (cli/exit (if ok? 0 1) exit-message)
      (case action
        "check-regex" (execute-with-output validate-regex args options)
        "check-nfa" (execute-with-output (partial validate-nfa false) args options)
        "check-dfa" (execute-with-output (partial validate-nfa true) args options)
        "check-dpda" (execute-with-output validate-dpda args options)
        "check-tm"   (execute-with-output validate-tm args options)
        "check-dtm"  (execute-with-output validate-dtm args options)
        "check-lba"  (execute-with-output validate-lba args options)
        "check-calc-dtm" (execute-with-output validate-calc-dtm args options)
        "check-loop-program" (execute-with-output validate-loop-program args options)
        "check-while-program" (execute-with-output validate-while-program args options)
        "check-goto-program" (execute-with-output validate-goto-program args options)))))
