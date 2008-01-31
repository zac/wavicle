;(in-package "ACL2")
(include-book "list-utilities" :dir :teachpacks)

;(include-book "ihs/ihs-definitions" :dir :system)
;(include-book "ihs/ihs-lemmas" :dir :system)

(include-book "binary-io-utilities" :dir :teachpacks)

(include-book "world" :dir :teachpacks)

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

(defun ascii->chrs (bytes)
  (if (consp bytes)
      (cons (code-char (car bytes)) (ascii->chrs (cdr bytes)))
      nil))

; (bytes->int bytes)
;    bytes - a list of bytes ordered in little endian order.
;  This function is a wrapper function for convertbytes->int that takes in
;  little-endian ordered bytes read from a WAV file and converts them to a
;  Single integer.
(defun bytes->integer (bytes)
  (if (= (length bytes) 2)
      (+ (car bytes) (* (cadr bytes) 256))
      (if (= (length bytes) 4)
          (+ (car bytes)
             (* (cadr bytes) 256)
             (* (caddr bytes) (* 256 256))
             (* (cadddr bytes) (* (* 256 256) 256)))
          -1)))

(defun parse-wav-file (bytes)
  (wav-file (chrs->str (ascii->chrs (subseq bytes 0 4))) ;chunk-id
            (bytes->integer (subseq bytes 4 8)) ;chunk-size
            (chrs->str (ascii->chrs (subseq bytes 8 12))) ;format
            (chrs->str (ascii->chrs (subseq bytes 12 16))) ;subchunk-1-id
            (bytes->integer (subseq bytes 16 20)) ;subchunk-1-size
            (bytes->integer (subseq bytes 20 22)) ;audio-format
            (bytes->integer (subseq bytes 22 24)) ;num-channels
            (bytes->integer (subseq bytes 24 28)) ;sample-rate
            (bytes->integer (subseq bytes 28 32)) ;byte-rate
            (bytes->integer (subseq bytes 32 34)) ;block-align
            (bytes->integer (subseq bytes 34 36)) ;bits-per-sample
            (chrs->str (ascii->chrs (subseq bytes 36 40))) ;subchunk-2-id
            (bytes->integer (subseq bytes 40 44)) ;subchunk-2-size
            (subseq bytes 44 (length bytes))))

(defun test-read (file state)
  (mv-let (bytes error state)
          (binary-file->byte-list file state)
          (mv (parse-wav-file bytes) error state)))