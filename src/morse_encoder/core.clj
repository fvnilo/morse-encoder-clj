(ns morse-encoder.core
  (:require
   [clojure.string :refer [upper-case join]]
   [dynne.sampled-sound :refer :all]))

(def alphabet {\A ".-" \B "-..." \C "-.-." \D "-.." \E "." \F"..-."
               \G "--." \H "..." \I ".." \J ".---" \K "-.-" \L ".-.."
               \M "--" \N "-." \O "---" \P ".--." \Q "--.-" \R ".-."
               \S "..." \T "-" \U "..-" \V "...-" \W ".--" \X "-..-"
               \Y "-.--" \Z "--.." \space "/"})

(def dit (sinusoid 0.2 440))
(def dah (sinusoid 0.6 440))
(def letter-space (silence 0.2 1))
(def word-space (silence 0.8 1))

(defn get-symbol-sound
  [symbol]
  (case symbol
    \. dit
    \- dah
    \/ word-space))

(defn break-to-chars
  [message]
  (-> (upper-case message)
       (char-array)
       (seq)))

(defn morse
  [message]
  (let [characters (break-to-chars message)]
    (map #(alphabet %) characters)))

(defn build-letter-sound
  [morse-letter]
  (let [symbols (break-to-chars morse-letter)
        sounds (map get-symbol-sound symbols)]
    (if (> (count sounds) 1)
      (append (reduce #(append (append %1 letter-space) %2) sounds) letter-space)
      (append (first sounds) letter-space))))

(def build-letter-sound-memoized (memoize build-letter-sound))

(defn build-words-sounds
  [morse-letters]
  (map build-letter-sound-memoized morse-letters))

(defn build-entire-message-sound
  [sounds]
  (reduce #(append %1 %2) sounds))


(defn encode-to-morse
  [message]
  (-> (morse message)
      (build-words-sounds)
      (build-entire-message-sound)))
