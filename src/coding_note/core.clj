(ns coding-note.core
  (:require [clojure.string :as string]))

;; lowercase alfabet
(def alfabet "abcdefghijklmnopqrstuvwxyz")

;; apply f to strings and concat result as string
(defn trans-str [f & ss]
  (string/join "" (apply map f ss)))

;; lowercase letter index in alfabet (starting from 0)
(defn idx-in-alfabet [letter]
  (- (int letter) (int \a)))

;; get char by index
(defn char-by-idx [idx]
  (char (+ (mod idx 26) (int \a))))

(defn caesar-shift [plain shift]
  (letfn [(transfrom-letter [letter sft]
            (if (some #{letter} alfabet)
                (char-by-idx (+ sft (idx-in-alfabet letter)))
                letter))]
    (->> (.toLowerCase plain)
         (trans-str #(transfrom-letter % shift)))))

(defn rail-fence [plain column]
  (->> (.toLowerCase plain)
       (partition column)
       (apply interleave)
       (string/join "")))

(defn vigenere-trans [letter key]
  (char-by-idx (+ (idx-in-alfabet letter)
                  (idx-in-alfabet key))))

(defn vigenere [plain key-str]
  (trans-str vigenere-trans plain (cycle key-str)))
