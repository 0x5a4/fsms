(ns fsms.nfa
  (:use [fsms.commons])
  (:require [instaparse.core :as insta]))

(defn initial-configurations [nfa word]
  (map (fn [s] {:state s :input word}) (get nfa :start)))

(defn next-states [nfa config]
  (let [next-input (->> config :input rest (apply str))
        next (get (:delta nfa) {:state (-> config :state)
                                :symbol (-> config :input first str)})]
    (map (fn [s] {:state s :input next-input}) next)))

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
  [{:state from :symbol sym} to])

(defn build-nfa [tree]
  (loop [[node & remain] tree
         start []
         final []
         deltaacc []]
    (if (not node)
      {:start (vec start)
       :final-states (distinct (vec final))
       :delta (update-vals (group-by first deltaacc) (partial map second))}
      (case (first node)
        :START (recur remain
                      (concat start (rest node))
                      final deltaacc)
        :FINAL (recur remain
                      start
                      (concat final (rest node))
                      deltaacc)
        :TRANS (recur remain start final
                      (conj deltaacc (trans-from-node node)))))))

(defn deterministic? [{:keys [delta start]}]
  (and
   (<= 0 (count start) 1)
   (not-any? (fn [[_ tos]] (not= 1 (count tos))) (seq delta))))

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
