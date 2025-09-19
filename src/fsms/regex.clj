(ns fsms.regex
  (:use [fsms.commons])
  (:require [instaparse.core :as insta]))

(defn- emptyset? [node]
  (= :EMPTYSET (first node)))

(def regex-parser
  (insta/parser
   "<REGEX> := WS (CONCAT | OR | KLEENE | LAMBDA | EMPTYSET | SYM) WS
    CONCAT := REGEX REGEX
    OR := <'('> REGEX  <'+'> REGEX <')'>
    KLEENE := <'('> REGEX <')*'>
    LAMBDA := <'_'>
    EMPTYSET := <'{}'>
    SYM := #'[^_/()+* {}]'
    <WS> := <#' '>*
    "))

(defn- sym-to-jregex [c]
  (str c))

(def ^:private lambda-to-jregex (constantly ""))

(defn- kleene-to-jregex [inner]
  (str "(" inner ")*"))

(defn- or-to-jregex [lhs rhs]
  (str "(" lhs "|" rhs ")"))

(defn- concat-to-jregex [lhs rhs]
  (str lhs rhs))

(defn- resolve-emptyset-lhs-rhs [node lhs rhs]
  (cond
    (and (emptyset? lhs) (emptyset? rhs)) [:EMPTYSET]
    (emptyset? lhs) rhs
    (emptyset? rhs) lhs
    :else [node lhs rhs]))

(defn- resolve-emptyset-kleene [inner]
  (if (emptyset? inner)
    [:LAMBDA]
    [:KLEENE inner]))

(def fold-emptyset {:CONCAT (partial resolve-emptyset-lhs-rhs :CONCAT)
                    :OR (partial resolve-emptyset-lhs-rhs :OR)
                    :KLEENE resolve-emptyset-kleene})

(def to-jregex {:CONCAT concat-to-jregex
                :OR or-to-jregex
                :KLEENE kleene-to-jregex
                :LAMBDA lambda-to-jregex
                :SYM sym-to-jregex})

(defn build-regex [parsed]
  (let [maybe-emptyset (insta/transform fold-emptyset parsed)]
    (if (-> maybe-emptyset first emptyset?)
      :EMPTYSET
      (->> maybe-emptyset
           (insta/transform to-jregex)
           first
           re-pattern))))

(defn accept? [regex word]
  (when (not= regex :EMPTYSET)
    (some? (re-matches regex word))))
