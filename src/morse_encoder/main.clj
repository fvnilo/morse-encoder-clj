(ns morse-encoder.main
  (:require
   [clojure.string :refer [trim-newline]]
   [morse-encoder.core :refer [to-morse to-sound-file]]))

(defn encode-and-output-wave
  [content]
  (let [morse-characters (to-morse content)]
    (println morse-characters)
    (to-sound-file morse-characters "result.wav")))

(defn -main
  "This should be pretty simple."
  [& args]
  (let [filename (first args)
        content (slurp (str filename))]
    (time (encode-and-output-wave (trim-newline content)))))
