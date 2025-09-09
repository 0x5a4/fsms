(ns fsms.integration-test
  (:use [fsms.core]
        [clojure.test])
  (:require [clojure.java.io :as io]))

; (deftest dpda-valid
;   (testing "DPDA is accepted as valid"
;     (is (= [] (validate-dpda (io/resource "dpda2.edn")
;                              (io/resource "dpda2-config.edn"))))))

; (deftest dpda-invalid
;   (testing "DPDA is accepted as valid"
;     (is (not= [] (validate-dpda (io/resource "dpda.edn")
;                                 (io/resource "dpda2-config.edn"))))))
