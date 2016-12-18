(ns encipher-note.core
  (:require [clojure.string :as string]
            [clojure.core.matrix :as mx]))

;; use for debug
(defn pp [& x]
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

(def playfair-matrix [[\c \i \p \h \e]
                      [\r \a \b \d \f]
                      [\g \k \l \m \n]
                      [\o \q \s \t \u]
                      [\v \w \x \y \z]])

(def playfair-map
  (into {} (for [x (range 5) y (range 5)]
                [(mx/mget playfair-matrix x y) [x y]])))

(defn playfair-pair [[c1 c2]]
  (letfn [(round-get [x y]
            (mx/mget playfair-matrix (mod x 5) (mod y 5)))
          (eq-row? [idx1 idx2]
            (apply = (map first [idx1 idx2])))
          (eq-col? [idx1 idx2]
            (apply = (map second [idx1 idx2])))]
    (let [c1-idx (playfair-map c1)
          c2-idx (playfair-map c2)]
      (cond (eq-row? c1-idx c2-idx)
            [(round-get (first c1-idx) (inc (second c1-idx)))
             (round-get (first c2-idx) (inc (second c2-idx)))]
            (eq-col? c1-idx c2-idx)
            [(round-get (inc (first c1-idx)) (second c1-idx))
             (round-get (inc (first c2-idx)) (second c2-idx))]
            :defaults
            [(round-get (first c1-idx) (second c2-idx))
             (round-get (first c2-idx) (second c1-idx))]))))

(defn pair-chars [[f s & other]]
  (cond (nil? f)
        (list)
        (nil? s)
        (cons f (cons (if (= f \x) \q \x) '()))
        (= f s)
        (cons f (cons (if (= f \x) \q \x)
                      (pair-chars (cons s other))))
        :default
        (cons f (cons s (pair-chars other)))))

(defn playfair [plain]
  (->> (string/replace plain #"j" "i")
       (pair-chars)
       (partition 2)
       (map playfair-pair)
       (flatten)
       (string/join "")))
