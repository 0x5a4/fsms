(ns fsms.automata.search)

(def MAX-STATES 1000)

(def ^:dynamic *debug* false)

(defn build-accept?-fn [init-fn successors-fn accept?-fn discard?-fn]
  (fn [automaton word]
    (loop [nr-states 0
           q (vec (init-fn automaton word))
           seen (set q)]
      ; (println (first q))
      (cond (empty? q) false
            (> nr-states MAX-STATES) (do (when *debug* (println "; INFO: reached maximum number of states, assuming non-accepting")) false)
            (accept?-fn automaton (first q)) (first q) ;; is truthy
            (discard?-fn (first q)) (do (when *debug* (println "; INFO: reached a limit, discarding configuration" (first q)))
                                        (recur nr-states (subvec q 1) seen))
            :else
            (let [config (first q)
                  succs (successors-fn automaton config)
                  succs (remove seen succs)]
              (recur (inc nr-states)
                     (into (subvec q 1) succs)
                     (into seen succs)))))))
