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

;(defun bytes->integer (bytes)
;  (if (= (length bytes) 2)
;      (+ (car bytes) (* (cadr bytes) 256))
;      (if (= (length bytes) 4)
;          (+ (car bytes)
;             (* (cadr bytes) 256)
;             (* (caddr bytes) (* 256 256))
;             (* (cadddr bytes) (* (* 256 256) 256)))
;          -1)))

;int is number to convert.
;num is number of bytes to convert to.
(defun integer->bytes (int num)
  (if (posp num)
      (cons (* (- (/ int 256) (floor int 256)) 256) (integer->bytes (floor int 256) (- num 1)))
      nil))

(defthm integer->bytes-thm
  (true-listp (integer->bytes lst n)))

;(36 8 0 0) = 2084

; (34 21 12 11) = 185341218

(defun nat-listp (lst)
  (if (endp lst)
      t
      (and (natp (car lst)) (nat-listp (cdr lst)))))

(defun shift-8-bits (l)
  (if (consp l)
      (cons (* (ifix (car l)) 256) (shift-8-bits (cdr l)))
      nil))

(defthm shift-8-bits-lemma
  (implies (true-listp lst) (nat-listp (shift-8-bits lst))))

(defun bytes->integer (bytes)
  (if (endp bytes)
      0
      (+ (bytes->integer (shift-8-bits (cdr bytes))) (car bytes))))

(defthm bytes->integer-thm
  (<= 0 (bytes->integer byte-list)))

(defun n-bytep (number bytes)
  (and (integerp number)
       (>= number 0)
       (<= number (- (expt 256 bytes) 1))))

(defthm integer->bytes->integer-thm
  (implies (n-bytep temp-int bytes)
           (equal temp-int (bytes->integer (integer->bytes temp-int bytes)))))

(defun parse-wav-file (bytes)
  (wav-file (subseq bytes 0 4) ;chunk-id
            (bytes->integer (subseq bytes 4 8)) ;chunk-size
            (subseq bytes 8 12) ;format
            (subseq bytes 12 16) ;subchunk-1-id
            (bytes->integer (subseq bytes 16 20)) ;subchunk-1-size
            (bytes->integer (subseq bytes 20 22)) ;audio-format
            (bytes->integer (subseq bytes 22 24)) ;num-channels
            (bytes->integer (subseq bytes 24 28)) ;sample-rate
            (bytes->integer (subseq bytes 28 32)) ;byte-rate
            (bytes->integer (subseq bytes 32 34)) ;block-align
            (bytes->integer (subseq bytes 34 36)) ;bits-per-sample
            (subseq bytes 36 40) ;subchunk-2-id
            (bytes->integer (subseq bytes 40 44)) ;subchunk-2-size
            (subseq bytes 44 (length bytes))))

;(defun parse-wav-file (bytes)
;  (wav-file (chrs->str (ascii->chrs (subseq bytes 0 4))) ;chunk-id
;            (bytes->integer (subseq bytes 4 8)) ;chunk-size
;            (chrs->str (ascii->chrs (subseq bytes 8 12))) ;format
;            (chrs->str (ascii->chrs (subseq bytes 12 16))) ;subchunk-1-id
;            (bytes->integer (subseq bytes 16 20)) ;subchunk-1-size
;            (bytes->integer (subseq bytes 20 22)) ;audio-format
;            (bytes->integer (subseq bytes 22 24)) ;num-channels
;            (bytes->integer (subseq bytes 24 28)) ;sample-rate
;            (bytes->integer (subseq bytes 28 32)) ;byte-rate
;            (bytes->integer (subseq bytes 32 34)) ;block-align
;            (bytes->integer (subseq bytes 34 36)) ;bits-per-sample
;            (chrs->str (ascii->chrs (subseq bytes 36 40))) ;subchunk-2-id
;            (bytes->integer (subseq bytes 40 44)) ;subchunk-2-size
;            (subseq bytes 44 (length bytes))))

(defun wav->byte-list (wav)
  (append (wav-file-chunk-id wav)
          (integer->bytes (wav-file-chunk-size wav) 4)
          (wav-file-format wav)
          (wav-file-subchunk-1-id wav)
          (integer->bytes (wav-file-subchunk-1-size wav) 4)
          (integer->bytes (wav-file-audio-format wav) 2)
          (integer->bytes (wav-file-num-channels wav) 2)
          (integer->bytes (wav-file-sample-rate wav) 4)
          (integer->bytes (wav-file-byte-rate wav) 4)
          (integer->bytes (wav-file-block-align wav) 2)
          (integer->bytes (wav-file-bits-per-sample wav) 2)
          (wav-file-subchunk-2-id wav)
          (integer->bytes (wav-file-subchunk-2-size wav) 4)
          (wav-file-data wav)))

(defun write-wav-file (wav path state)
  (byte-list->binary-file path (wav->byte-list wav) state))

(defun modify-data (wav new-data)
  (wav-file (wav-file-chunk-id wav) ;chunk-id
            (wav-file-chunk-size wav) ;chunk-size
            (wav-file-format wav) ;format
            (wav-file-subchunk-1-id wav) ;subchunk-1-id
            (wav-file-subchunk-1-size wav) ;subchunk-1-size
            (wav-file-audio-format wav) ;audio-format
            (wav-file-num-channels wav) ;num-channels
            (wav-file-sample-rate wav) ;sample-rate
            (wav-file-byte-rate wav) ;byte-rate
            (wav-file-block-align wav) ;block-align
            (wav-file-bits-per-sample wav) ;bits-per-sample
            (wav-file-subchunk-2-id wav) ;subchunk-2-id
            (wav-file-subchunk-2-size wav) ;subchunk-2-size
            new-data))

(defun half-vals (data packet-size)
  (if (endp data)
      nil
      (append (integer->bytes (floor (bytes->integer (subseq data 0 packet-size)) 2) packet-size) (half-vals (subseq data packet-size (length data)) packet-size))))

(defun write-wav (data path state)
  (mv-let (error state)
          (write-wav-file data path state)
          (mv "SUCCES" state)))

(defun test-read (file state)
  (mv-let (bytes error state)
          (binary-file->byte-list file state)
          (let ((wav (parse-wav-file bytes)))
            (write-wav (modify-data wav (half-vals (wav-file-data wav) (wav-file-block-align wav))) "/Users/zac/Desktop/output.wav" state))))