(in-package "ACL2")

(include-book "audio" :dir :teachpacks)

(include-book "list-utilities" :dir :teachpacks)
;(include-book "operators")
;(include-book "ihs/ihs-definitions" :dir :system)
;(include-book "ihs/ihs-lemmas" :dir :system)

(include-book "binary-io-utilities" :dir :teachpacks)

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
      (integer->bytes-h (+ int 128) num)))

;(36 8 0 0) = 2084

; (34 21 12 11) = 185341218

(defun nat-listp (lst)
  (if (endp lst)
      t
      (and (natp (car lst)) (nat-listp (cdr lst)))))

(defun shift-8-bits (l)
  (declare (xargs :guard (integer-listp l)
                  :verify-guards t))
  (if (consp l)
      (cons (ifix (abs (* (car l) 256))) (shift-8-bits (cdr l)))
      nil))

(defthm shift-8-bits-reduces
  (implies (and (true-listp lst) (consp lst)) (< (length (shift-8-bits (cdr lst))) (length lst))))

(defthm shift-8-bits-truelist
  (implies (true-listp lst) (true-listp (multiply-list num lst))))

(defun bytes->integer-h (bytes)
  (declare (xargs :guard (integer-listp bytes)
                  :verify-guards t))
  (if (endp bytes)
      0
      (+ (car bytes) (bytes->integer-h (shift-8-bits (cdr bytes))))))

(defun bytes->integer (bytes)
  (let ((val (bytes->integer-h bytes)))
    (if (> (length bytes) 1)
        (unsigned->2scomp val (* 8 (length bytes)))
        (- val 128))))

(defun n-bytep (number bytes)
  (and (integerp number)
       (>= number 0)
       (<= number (- (expt 256 bytes) 1))))

;;; Converts a byte list into a list of normalized rationals. One for each sample and channel.
;(defun bytes->samples (data sample-size)
;  (if (endp data)
;      nil
;      (cons (/ (bytes->integer (take sample-size data)) (expt 2 (- (* 8 sample-size) 1))) (bytes->samples (nthcdr sample-size data) sample-size))))

;; Converts a byte list into a list of normalized rationals. One for each sample and channel.
(defun bytes->samples (data sample-size acc)
  (if (endp data)
      acc
      (bytes->samples (nthcdr sample-size data) sample-size (cons (/ (bytes->integer (take sample-size data)) (expt 2 (- (* 8 sample-size) 1))) acc))))

(defun samples->bytes (samples block-align)
  (if (endp samples)
      nil
      (append (integer->bytes (floor (* (car samples) (expt 2 (- (* 8 block-align) 1))) 1) block-align) (samples->bytes (cdr samples) block-align))))

;; TAKES 50.2 seconds to run on voice5.wav.
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
            (bytes->samples (nthcdr 44 bytes) (bytes->integer (subseq bytes 32 34)) nil)))

;; TAKES 20.4 seconds to run on voice5.wav.
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

(defun write-wav (data path state)
  (mv-let (error state)
          (write-wav-file data path state)
          (mv "SUCCESS" state)))

(defun read-wav (file state)
  (mv-let (bytes error state)
          (binary-file->byte-list file state)
          (mv (parse-wav-file bytes) state)))