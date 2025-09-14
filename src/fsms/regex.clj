(ns fsms.regex
  (:use [fsms.commons])
  (:require [instaparse.core :as insta]))

(def regex-parser
  (insta/parser
   "<REGEX> := WS (CONCAT | OR | KLEENE | LAMBDA | SYM) WS
    CONCAT := REGEX REGEX
    OR := <'('> REGEX  <'+'> REGEX <')'>
    KLEENE := <'('> REGEX <')*'>
    LAMBDA := <'_'>
    SYM := #'[^_/()+* ]'
    <WS> := <#' '>*
    "))

(defn- transform-sym [c]
  (str c))

(def transform-lambda (constantly ""))

(defn- transform-kleene [inner]
  (str "(" inner ")*"))

(defn- transform-or [lhs rhs]
  (str "(" lhs "|" rhs ")"))

(defn- transform-concat [lhs rhs]
  (str lhs rhs))

(defn build-regex [regex-string]
  (->>
   regex-string
   regex-parser
   (insta/transform {:CONCAT transform-concat
                     :OR transform-or
                     :KLEENE transform-kleene
                     :LAMBDA transform-lambda
                     :SYM transform-sym})
   first
   re-pattern))

(defn file->regex [file]
  (-> file slurp build-regex))

(defn accept? [regex word]
  (some? (re-matches regex word)))
