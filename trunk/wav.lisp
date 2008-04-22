(in-package "ACL2")

(include-book "list-utilities" :dir :teachpacks)
(include-book "binary-io-utilities" :dir :teachpacks)

; (unsigned->2scomp num numbits)
; This function determines the 2's Compliment value of an unsigned series of
; bits that is numbits in length.
; num = an unsigned integer to be converted.
; numbits = the desired number of bits. Used to determine the sign bit.
(defun unsigned->2scomp (num numbits)
  (let ((pivot (expt 2 (- numbits 1))))
    (if (< num pivot)
        num
        (- 0 (- pivot (- num pivot))))))

; (2scomp->unsigned num numbits)
; This function determines the unsigned value of an 2's Compliment series of
; bits that is numbits in length.
; num = an unsigned integer to be converted.
; numbits = the desired number of bits. Used to determine the sign bit.
(defun 2scomp->unsigned (num numbits)
  (let ((pivot (expt 2 (- numbits 1))))
    (if (>= num 0)
        num
        (+ pivot (- pivot (- 0 num))))))

; (bytes->integer-h bytes)
; Helper function that does all the heavy lifting for integer->bytes.
; int = number to convert.
; num = number of bytes to convert to.
(defun integer->bytes-h (int num)
  (if (posp num)
      (cons (* (- (/ int 256) (floor int 256)) 256)
            (integer->bytes-h (floor int 256) (- num 1)))
      nil))

; (integer->bytes int num)
; Takes a number and convert it to a byte list accounting for two's complement.
; int = number to convert.
; num = number of bytes to convert to.
(defun integer->bytes (int num)
  (if (> num 1)
      (integer->bytes-h (2scomp->unsigned int (* 8 num)) num)
      (integer->bytes-h (+ int 128) num)))

; (shift-8-bits l)
; Multiplies every item in l by 256 to shift the bits.
; l = the list of bytes to shift.
(defun shift-8-bits (l)
  (declare (xargs :guard (integer-listp l)
                  :verify-guards t))
  (if (consp l)
      (cons (ifix (abs (* (car l) 256))) (shift-8-bits (cdr l)))
      nil))

; (bytes->integer-h bytes)
; Helper function that does all the heavy lifting for bytes->integer.
; bytes = the bytes to be converted.
(defun bytes->integer-h (bytes count)
  (if (consp bytes)
      (+ (* (car bytes) (expt 256 count)) (bytes->integer-h (cdr bytes) (+ count 1)))
      0))

; (bytes->integer bytes)
; Takes a byte list and turns it into a number accounting for
; two's complement.
; bytes = the bytes to be converted.
(defun bytes->integer (bytes)
  (let ((val (bytes->integer-h bytes 0)))
    (if (> (length bytes) 1)
        (unsigned->2scomp val (* 8 (length bytes)))
        (- val 128))))

; (bytes->samples data sample-size acc)
; Converts a byte list into a list of normalized rationals. One for each
; sample and channel.
; data = the bytes to be turned into a samples list.
; sample-size = the number of bytes for each sample.
; acc = holds the tail-recursive list accumulator.
(defun bytes->samples (data sample-size acc)
  (if (endp data)
      (reverse acc)
      (bytes->samples (nthcdr sample-size data) sample-size
                      (cons (/ (bytes->integer (take sample-size data))
                               (expt 2 (- (* 8 sample-size) 1))) acc))))

; (samples->bytes samples block-align)
; Takes a list of samples from -1 to 1 and converts it to a list of bytes.
; samples = the list of samples to convert.
; block-align = the block align of the data.
(defun samples->bytes (samples block-align)
  (if (endp samples)
      nil
      (append (integer->bytes (floor (* (car samples)
                                        (expt 2 (- (* 8 block-align) 1))) 1)
                              block-align)
              (samples->bytes (cdr samples) block-align))))

;; TAKES 50.2 seconds to run on voice5.wav.

; (parse-wav-file bytes)
; Converts a list of bytes into a wav structure.
; bytes = the bytes read in from a file.
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
            (bytes->samples (nthcdr 44 bytes)
                            (bytes->integer (subseq bytes 32 34))
                            nil)))

;; TAKES 20.4 seconds to run on voice5.wav.

; (wav->byte-list wav)
; Converts a wav structure into a list of bytes by calling all the necessary
; functions to convert each field.
; wav = the wav structure to be turned into a list.
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

; (write-wav-file wav path state)
; Wrapper to call byte-list->binary-file with the parameters.
; wav = the wav structure to be written to a file.
; path = the path to be written to.
; state = ACL2 state.
(defun write-wav-file (wav path state)
  (byte-list->binary-file path (wav->byte-list wav) state))

; (write-wav data path state)
; Wrapper to call write-wav-file with the parameters.
; data = the wav structure to be written to a file.
; path = the path to be written to.
; state = ACL2 state.
(defun write-wav (data path state)
  (mv-let (error state)
          (write-wav-file data path state)
          (mv "SUCCESS" state)))

; (read-wav-file file state)
; Gets the byte list from the input file and parses it into a structure.
; file = the path to the file being read in.
; state = ACL2 state.
(defun read-wav (file state)
  (mv-let (bytes error state)
          (binary-file->byte-list file state)
          (mv (parse-wav-file bytes) state)))