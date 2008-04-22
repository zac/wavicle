(in-package "ACL2")

;(include-book "world" :dir :teachpacks)

(defstructure wav-file
  (chunk-id (:assert (stringp chunk-id)))
  (chunk-size (:assert (integerp chunk-size)))
  (format (:assert (stringp format)))
  (subchunk-1-id (:assert (listp subchunk-1-id)))
  ;Make sure it is PCM.
  (subchunk-1-size (:assert (integerp subchunk-1-size)))
  (audio-format (:assert (= audio-format 1)))
  ;Mono = 1, Stereo = 2, etc.
  (num-channels (:assert (integerp num-channels)))
  ;8000, 44100, etc.
  (sample-rate (:assert (integerp sample-rate)))
  ;== sample-rate * num-channels * (bits-per-sample / 8)
  (byte-rate (:assert (integerp byte-rate)))
  ;== num-channels * (bits-per-sample / 8)
  (block-align (:assert (integerp block-align)))
  ;8 bits = 8, 16 bits = 16, etc.
  (bits-per-sample (:assert (integerp bits-per-sample)))
  (subchunk-2-id (:assert (listp subchunk-2-id)))
  (subchunk-2-size (:assert (integerp subchunk-2-size)))
  (data (:assert (listp data))))

;Structure that contains the required information to run the program.
(defstructure acf
  (output       (:assert (string-listp output)))
  (commands     (:assert (true-listp commands))))