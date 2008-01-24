(include-book "binary-io-utilities" :dir :teachpacks)
(include-book "list-utilities" :dir :teachpacks)

(defstructure wav-file
  (chunk-id (:assert (stringp chunk-id)))
  (chunk-size (:assert (integerp chunk-id)))
  (format (:assert (stringp format)))
  (subchunk-1-id (:assert (listp subchunk-1-id)))
  ;Make sure it is PCM.
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

(defun bytes->integer (bytes type)
  0)

(defun parse-wav-file (bytes)
  (wav-file (chrs->str (ascii->chrs (subseq bytes 0 4)))
            (bytes->integer (subseq bytes 4 8))
            (chrs->str (ascii->chrs (subseq bytes 8 12)))
            0
            0
            0
            0
            0
            0
            0
            0
            0
            0))

(defun test-read (file state)
  (mv-let (bytes error state)
          (binary-file->byte-list file state)
          (mv (parse-wav-file bytes) error state)))