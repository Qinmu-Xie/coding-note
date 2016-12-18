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

(deftest hill-cipher-test
  (testing "Should HILL CIPHER work"
    (is (= (hill-encipher "hithere" [[1 3] [0 2]])
           "irrpgjtj"))
    (is (= (hill-encipher "WEREDONE" [[1 7] [0 3]])
           "foaoeswo"))))

(deftest play-fair-test
  (testing "playfair-map take a char and return an index"
    (is (= (playfair-map \a) [1 1]))
    (is (= (playfair-map \q) [3 1]))
    (is (= (playfair-map \c) [0 0]))
    (is (= (playfair-map \z) [4 4])))
  (testing "playfair-pair takes two char and return ciphered chars"
    (is (= (playfair-pair [\h \e]) [\e \c]))
    (is (= (playfair-pair [\d \m]) [\m \t]))
    (is (= (playfair-pair [\k \t]) [\m \q]))
    (is (= (playfair-pair [\o \d]) [\t \r])))
  (testing "should PLAYFAIR work"
    (is (= (playfair "ballon")
           "dbspgslz"))
    (is (= (playfair "wherethereislifethereishope")
           "yicfhuecfcpqkpnfydcfcptpscpz"))))
