;(in-package "ACL2")
(include-book "list-utilities" :dir :teachpacks)

;(include-book "ihs/ihs-definitions" :dir :system)
;(include-book "ihs/ihs-lemmas" :dir :system)

(include-book "binary-io-utilities" :dir :teachpacks)

(include-book "world" :dir :teachpacks)

(include-book "operators")

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

; (unsigned->2scomp num numbits)
;    num - An unsigned integer to be converted.
;    numbits - The desired number of bits. Used to determine the sign bit.
;  This function determines the 2's Compliment value of an unsigned series of
;  bits that is numbits in length.
(defun unsigned->2scomp (num numbits)
  (let ((pivot (expt 2 (- numbits 1))))
    (if (< num pivot)
        num
        (- 0 (- pivot (- num pivot))))))

; (2scomp->unsigned num numbits)
;    num - An unsigned integer to be converted.
;    numbits - The desired number of bits. Used to determine the sign bit.
;  This function determines the unsigned value of an 2's Compliment series of
;  bits that is numbits in length.
(defun 2scomp->unsigned (num numbits)
  (let ((pivot (expt 2 (- numbits 1))))
    (if (posp num)
        num
        (+ pivot (- pivot (- 0 num))))))

;int is number to convert.
;num is number of bytes to convert to.
(defun integer->bytes-h (int num)
  (if (posp num)
      (cons (* (- (/ int 256) (floor int 256)) 256) (integer->bytes-h (floor int 256) (- num 1)))
      nil))

(defun integer->bytes (int num)
  (if (> num 1)
      (integer->bytes-h (2scomp->unsigned int (* 8 num)) num)
      (integer->bytes-h int num)))

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

(defun bytes->integer-h (bytes)
  (if (endp bytes)
      0
      (+ (bytes->integer-h (shift-8-bits (cdr bytes))) (car bytes))))

(defun bytes->integer (bytes)
  (let ((val (bytes->integer-h bytes)))
    (if (> (length bytes) 1)
        (unsigned->2scomp val (* 8 (length bytes)))
        val)))

(defthm bytes->integer-thm
  (<= 0 (bytes->integer byte-list)))

(defun n-bytep (number bytes)
  (and (integerp number)
       (>= number 0)
       (<= number (- (expt 256 bytes) 1))))

(defthm integer->bytes->integer-thm
  (implies (n-bytep temp-int bytes)
           (equal temp-int (bytes->integer (integer->bytes temp-int bytes)))))

;; Converts a byte list into a list of normalized rationals. One for each sample and channel.
(defun bytes->samples (data sample-size)
  (if (endp data)
      nil
      (cons (/ (bytes->integer (subseq data 0 sample-size)) (expt 2 (- (* 8 sample-size) 1))) (bytes->samples (nthcdr sample-size data) sample-size))))

(defun samples->bytes (samples block-align)
  (if (endp samples)
      nil
      (append (integer->bytes (floor (* (car samples) (expt 2 (- (* 8 block-align) 1))) 1) block-align) (samples->bytes (cdr samples) block-align))))

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
            (bytes->samples (nthcdr 44 bytes) (bytes->integer-h (subseq bytes 32 34)))))

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
          (samples->bytes (wav-file-data wav) (wav-file-block-align wav))))

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

;(defun half-vals (data packet-size)
;  (if (endp data)
;      nil
;      (append (integer->bytes (* (bytes->integer (subseq data 0 packet-size)) 2) packet-size) (half-vals (subseq data packet-size (length data)) packet-size))))

(defun mod-vals (samples)
  (if (endp samples)
      nil
      (cons (* (car samples) 2) (mod-vals (cdr samples)))))

(defun fuzz-h (h g data)
  (if (endp data)
      nil
      (let ((f (* g h)))
        (cons (max (- f) (min f (car data))) (fuzz-h h g (cdr data))))))
; (maximum list)
;   Gives the maximum element of the list.
;   list = list of numbers to compare.
; (maximum '(1 2 43 54 23 11 32 55 33)) -> 55

(defun maximum (list)
  (if (consp (cdr list))
      (max (car list) (maximum (cdr list)))
      (car list)))

(defun fuzz (val wav)
  (let* ((data (wav-file-data wav))
         (g (maximum data)))
    (modify-data wav (fuzz-h val g (wav-file-data wav)))))

(defun normalize-data (data maximum)
  (if (endp data)
      nil
      (cons (/ (car data) maximum) (normalize-data (cdr data) maximum))))

(defun write-wav (data path state)
  (mv-let (error state)
          (write-wav-file data path state)
          (mv "SUCCES" state)))

(defun test-mod (file output state)
  (mv-let (bytes error state)
          (binary-file->byte-list file state)
          (let ((wav (parse-wav-file bytes)))
            (write-wav (fuzz 1/16 wav) output state))))

(defun read-wav (file state)
  (mv-let (bytes error state)
          (binary-file->byte-list file state)
          (parse-wav-file bytes)))