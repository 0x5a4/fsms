(ns fsms.automata.turing-machine
  (:use [fsms.commons])
  (:require [instaparse.core :as insta]))

(def MAX-BAND-LENGTH 30)

(def blank lambda)

(defn initial-configurations [tm word]
  [{:state (get tm :start)
    :band-left blank
    :current (str (first word))
    :band-right (let [s (apply str (rest word))] (if (empty? s) blank s))}])

(defn turing-step
  "Executes a single step of a turing machine."
  [tm config]
  (let [delta (:delta tm)
        transs (get delta {:state (get config :state) :symbol (get config :current)})]
    (for [{:keys [direction symbol] :as trans} transs
          :let [{:keys [state band-left current band-right]} config]]
      {:state (get trans :state)
       :band-left (case direction
                    "L" (let [b' (apply str (butlast band-left))] (if (empty? b') blank b'))
                    "N" band-left
                    "R" (str band-left symbol))
       :current (case direction
                  "L" (str (last band-left))
                  "N" (get trans :symbol)
                  "R" (str (first band-right)))
       :band-right (case direction
                     "L" (apply str symbol band-right)
                     "N" band-right
                     "R" (let [b' (apply str (rest band-right))] (if (empty? b') blank b')))})))

(defn initial-lba-configurations [tm word]
  (let [word (update (vec word) (dec (count word)) (comp (:symbols tm) str))]
    [{:state (get tm :start)
      :band-left ""
      :current (str (first word))
      :band-right (apply str (rest word))}]))

(defn lba-step
  "Executes a single step of an LBA."
  [tm config]
  (when-not (:current config)
    (throw (IllegalStateException. "LBA attempted read outside of its input space")))
  (let [delta (:delta tm)
        transs (get delta {:state (get config :state) :symbol (get config :current)})]
    (for [{:keys [direction symbol] :as trans} transs
          :let [{:keys [state band-left current band-right]} config]]
      {:state (get trans :state)
       :band-left (case direction
                    "L" (apply str (butlast band-left))
                    "N" band-left
                    "R" (str band-left symbol))
       :current (case direction
                  "L" (and (last band-left) (str (last band-left)))
                  "N" (get trans :symbol)
                  "R" (and (first band-right) (str (first band-right))))
       :band-right (case direction
                     "L" (apply str symbol band-right)
                     "N" band-right
                     "R" (apply str (rest band-right)))})))

(defn turing-accepting?
  [tm config]
  (contains? (get tm :final-states) (get config :state)))

(defn turing-discard? [config]
  (< MAX-BAND-LENGTH (+ (count (get config :band-left))
                        1
                        (count (get config :band-right)))))

(defn assert-deterministic [tm]
  (let [delta (:delta tm)]
    (assert (<= (reduce max 0 (map count (vals delta))) 1))))

(defn result-from-configuration [config]
  (->> (concat (:band-left config) [(:current config)] (:band-right config))
       (drop-while #{(first blank)})
       reverse
       (drop-while #{(first blank)})
       reverse
       (apply str)))

(def tm-parser
  (insta/parser
   "<TURING> := DEF (BREAK DEF)*
    <DEF> := WS (START | FINAL | TRANS | SYMBOLS)? WS COMMENT?
    <COMMENT> := <';'> <#'[^\\n]*'>
    TRANS := LBRACK STATE COMMA SYM RBRACK ARROW LBRACK STATE COMMA SYM COMMA HEAD RBRACK
    SYMBOLS := <'symbols'> (LBRACK SYM COMMA SYM RBRACK)+
    START := <'start'> WS STATE
    FINAL := <'final'> (WS STATE)+
    <HEAD> := 'L' | 'N' | 'R'
    <SYM> := #'[^\\s,()]'
    <LBRACK> := WS <'('> WS
    <RBRACK> := WS <')'> WS
    <ARROW> := WS <'->'> WS
    <COMMA> := WS <','> WS
    <STATE> := #'[a-zA-Z_0-9]+'
    <BREAK> := <'\\n'>
    <WS> := <#' '>*"))

(defn- trans-from-node [[_ from sym to write direction]]
  [{:state from :symbol sym} {:state to :symbol write :direction direction}])

(defn build-tm [tree]
  (loop [[node & remain] tree
         start nil
         final #{}
         symbols {"_" "_"}
         deltaacc []]
    (if (not node)
      {:start start
       :final-states (set final)
       :symbols symbols
       :delta (update-vals (group-by first deltaacc) (partial map second))}
      (case (first node)
        :START (recur remain
                      (last node)
                      final symbols deltaacc)
        :FINAL (recur remain start
                      (concat final (rest node))
                      symbols deltaacc)
        :SYMBOLS (recur remain start final
                        (into symbols (map vec (partition 2 (rest node))))
                        deltaacc)
        :TRANS (recur remain start final symbols
                      (conj deltaacc (trans-from-node node)))))))

(defn validate [{:keys [start final-states symbols] :as tm} require-symbols]
  (assert start "PARSE CRITICIAL: expected a start state")
  (assert (not-empty final-states) "PARSE CRITICIAL: expected at least one final state")
  (when require-symbols
    (assert (not-empty symbols)
            "PARSE CRITICAL: expected at least one symbols declaration"))
  tm)
