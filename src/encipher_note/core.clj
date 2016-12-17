(ns encipher_note.core
  (:require [clojure.string :as string]
            [clojure.core.matrix :as mx]))

;; use for debug
(defn pp [x]
  (do (prn x) x))

;; lowercase alphabet
(def alphabet "abcdefghijklmnopqrstuvwxyz")

;; make f only work on alphabet letter
(defn letter-change [f]
  (fn [letter & args]
    (if (some #{(java.lang.Character/toLowerCase letter)} alphabet)
        (apply f letter args)
        letter)))

;; apply f to letter in strings and concat result as string
(defn trans-str [f & ss]
  (string/join "" (apply map (letter-change f) ss)))

;; lowercase letter index in alphabet (starting from 0)
(defn idx-in-alphabet [letter]
  (- (int (java.lang.Character/toLowerCase letter))
     (int \a)))

(defn letter2number [letter]
  (inc (idx-in-alphabet letter)))

;; get char by index
(defn char-by-idx [idx]
  (char (+ (mod idx 26) (int \a))))

(defn number2letter [number]
  (char-by-idx (dec number)))

(defn caesar-shift [plain shift]
  (trans-str #(char-by-idx (+ shift (idx-in-alphabet %)))
             plain))

(defn rail-fence [plain column]
  (->> plain
       (partition column)
       (apply interleave)
       (string/join "")))

(defn vigenere [plain key-str]
  (letfn [(_vigenere [letter key]
            (char-by-idx (+ (idx-in-alphabet letter)
                            (idx-in-alphabet key))))]
    (trans-str _vigenere plain (cycle key-str))))

;; factor should be one of 3,5,7,9,11,15,17,19,21,23,25
(defn multiplication-shift [plain factor]
  (trans-str #(char-by-idx (* factor (idx-in-alphabet %)))
             plain))

;; combination of caesar & multiplication
(defn affine-shift [plain factor shift]
  (trans-str #(char-by-idx (+ shift (* factor (idx-in-alphabet %))))
             plain))

(defn hill-encipher
  ([plain hill-matrix]
   (hill-encipher plain hill-matrix \e))
  ([plain hill-matrix pad]
   (let [n (count (first hill-matrix))]
     (->> (.toLowerCase plain)
          (partition n n [pad])
          (#(interleave % %))
          (map #(map letter2number %))
          (map #(apply + (apply map * %&)) (cycle hill-matrix))
          (map number2letter)
          (string/join "")))))
