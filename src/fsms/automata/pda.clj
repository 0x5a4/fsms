(ns fsms.automata.pda
  (:use [fsms.commons]
        [instaparse.core :as insta])
  (:require [clojure.string :as s]))

(def MAX-STACKSIZE 30)

(defn initial-configurations [pda word]
  [{:state (get pda :start)
    :input word
    :stack "#"}])

(defn trans [config sym next-state]
  (when next-state
    (-> config
        (assoc :state (:state next-state))
        (update :input (if (= sym lambda) identity (comp (partial apply str) rest)))
        ;; NOTE: An empty / lambda stack is represented as empty string (as returned by the parser). 
        (update :stack (fn [stack] (apply str (concat (:new-stack next-state) (rest stack))))))))

(defn next-states [pda config]
  (if (empty? (:stack config))
    nil
    (let [sym (str (first (get config :input)))
          consume-sym (get-in pda [:delta {:state (get config :state)
                                           :symbol sym
                                           :top-of-stack (str (first (get config :stack)))}])
          lambda-trans (get-in pda [:delta {:state (get config :state)
                                            :symbol lambda
                                            :top-of-stack (str (first (get config :stack)))}])]
      (concat (map (partial trans config sym) consume-sym)
              (map (partial trans config lambda) lambda-trans)))))

(defn dpda-accepting-configuration? [dpda config]
  (and (empty? (:input config))
       (contains? (get dpda :final-states) (get config :state))))

(defn discard-config? [config]
  (< MAX-STACKSIZE (count (get config :stack))))

(def pda-parser
  (insta/parser
   "<PDA> := DEF (BREAK DEF)*
    <DEF> := WS (START | FINAL | TRANS)? WS
    TRANS := LBRACK STATE COMMA SYM COMMA SYM RBRACK ARROW LBRACK STATE COMMA SYM+ RBRACK
    START := <'start'> WS STATE
    FINAL := <'final'> (WS STATE)+
    <SYM> := #'[a-zA-Z0-9_#]'
    <LBRACK> := WS <'('> WS
    <RBRACK> := WS <')'> WS
    <ARROW> := WS <'->'> WS
    <COMMA> := WS <','> WS
    <STATE> := #'[a-zA-Z0-9_\\-]+'
    <BREAK> := <'\\n'>
    <WS> := <#' '>*"))

(defn- trans-from-node [[_ from sym pop to push]]
  [{:state from :symbol sym :top-of-stack pop}
   {:state to :new-stack (s/replace push lambda "")}])

(defn build-pda [tree]
  (loop [[node & remain] tree
         start nil
         final []
         deltaacc []]
    (if (not node)
      {:start start
       :final-states (distinct (vec final))
       :delta (update-vals (group-by first deltaacc) (partial map second))}
      (case (first node)
        :START (recur remain
                      (last node)
                      final deltaacc)
        :FINAL (recur remain
                      start
                      (concat final (rest node))
                      deltaacc)
        :TRANS (recur remain start final
                      (conj deltaacc (trans-from-node node)))))))

(defn validate-deterministic [{:keys [delta]}]
  (assert (not-any? (fn [[_ tos]] (not= 1 (count tos))) (seq delta)) "CRITICAL: not deterministic, multiple transitions have the same right hand side")
  (doseq [[{:keys [state symbol top-of-stack]}] delta]
    (assert (if (= symbol lambda)
              (contains? delta {:state state :symbol lambda :top-of-stack top-of-stack})
              true)
            (format "CRITICAL: not deterministc, both right hand sides (%s, %s, %s) and (%s, _, %s) are defined"
                    state
                    symbol
                    top-of-stack
                    state
                    top-of-stack))))

(defn validate [{:keys [start final-states delta] :as nfa} deterministic]
  (assert start "PARSE CRITICIAL: expected a start state")
  (assert (not-empty final-states) "PARSE CRITICIAL: expected at least one final state")
  (assert (not-any? #(= (:symbol %) lambda) delta)
          "CRITICAL: transition function has lambda transition(s)")
  (when deterministic (validate-deterministic nfa)))

(defn file->pda [file deterministic]
  (let [pda (-> file
                slurp
                pda-parser
                build-pda)]
    (validate pda deterministic)
    pda))
