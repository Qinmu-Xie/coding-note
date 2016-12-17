(ns encipher-note.core-test
  (:require [clojure.test :refer :all]
            [encipher-note.core :refer :all]))

(deftest caesar-shift-test
  (testing "Should CAESAR work"
    (is (= (caesar-shift "xyz" 1) "yza"))
    (is (= (caesar-shift "im glad we met" -1)
           "hl fkzc vd lds"))))

(deftest rail-fence-test
  (testing "should RAIL-FENCE work"
    (is (= (rail-fence "thelongestdaymusthaveanend" 2)
           "teogsdyutaennhlnetamshvaed"))
    (is (= (rail-fence "primedifferencebetweenelementsresmonsibleforhiroshimaandnagasaki" 8)
           "pfeesesnretmmfhairweooigmeennrmaenetshasdcnsiiaaieerbrnkfblelodi"))))

(deftest vigenere-test
  (testing "should VIGENERE work"
    (is (= (vigenere "Tobeornottobethatisthequestion"
                     "have")
           "aowivrisatjfltceainxoelylsomvn"))))

(deftest multiplication-shift-test
  (testing "Should MULTIPLICATION work"
    (is (= (multiplication-shift "xy z" 3) "ru x"))))

(deftest affine-shift-test
  (testing "Should AFFINE work"
    (is (= (affine-shift "xy z" 3 3) "ux a"))))
