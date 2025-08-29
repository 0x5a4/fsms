(ns fsms.nfa
  (:use [fsms.commons])
  (:require [instaparse.core :as insta]))

(defn initial-configurations [nfa word]
  (map (fn [x] {:state x :input word}) (get nfa :start)))

(defn next-states [nfa config]
  (let [next-input (->> config :input rest (apply str))
        transitions (filter (fn [{:keys [state symbol]}]
                              (and
                               (= state (-> config :state))
                               (= symbol (-> config :input first str)))) (:delta nfa))]
    (map (fn [{:keys [to]}]
           {:state to :input next-input}) transitions)))

(defn accept? [nfa config]
  (and (empty? (:input config))
       (some #{(get config :state)} (get nfa :final-states))))

(def discard? (constantly false))

(def nfa-parser
  (insta/parser
   "<NFA> := DEF (BREAK DEF)*
    <DEF> := WS (START | FINAL | TRANS)? WS
    TRANS := <'('> WS STATE WS <','> WS SYM WS <')'> WS <'->'> WS STATE
    START := <'start'> (WS STATE)+
    FINAL := <'final'> (WS STATE)+
    <SYM> := #'[a-zA-Z0-9_]'
    <STATE> := #'[a-zA-Z0-9_\\-]+'
    <BREAK> := <'\\n'>
    <WS> := <#' '>*"))

(defn- trans-from-node [[_ from sym to]]
  {:state from :symbol sym :to to})

(defn build-nfa [tree]
  (loop [[node & remain] tree
         start []
         final []
         delta []]
    (if (not node)
      {:start (distinct (vec start))
       :final-states (distinct (vec final))
       :delta (distinct delta)}
      (case (first node)
        :START (recur remain
                      (concat start (rest node))
                      final delta)
        :FINAL (recur remain
                      start
                      (concat final (rest node))
                      delta)
        :TRANS (recur remain start final
                      (conj delta (trans-from-node node)))))))

(defn deterministic? [{:keys [delta start]}]
  (and
   (<= 0 (count start) 1)
   (every? (fn [{:keys [state symbol to]}]
             (not (some #(and (= state (:state %))
                              (= symbol (:symbol %))
                              (not= to (:to %))) delta))) delta)))

(defn validate [{:keys [start final-states delta] :as nfa} deterministic]
  (assert (not-empty start) "PARSE CRITICIAL: expected at least one start state")
  (assert (not-empty final-states) "PARSE CRITICIAL: expected at least one final state")
  (assert (not-any? #(= (:symbol %) lambda) delta)
          "CRITICAL: transition function has lambda transition(s)")
  (when deterministic (assert (deterministic? nfa)
                              "CRITICAL: nfa is not deterministic")))

(defn file->nfa [file deterministic]
  (let [nfa (-> file
                slurp
                nfa-parser
                build-nfa)]
    (validate nfa deterministic)
    nfa))
