(in-package "ACL2")
(include-book "doublecheck" :dir :teachpacks)
(include-book "structures")
(include-book "operators")
(include-book "wav")
(include-book "filter")

;; Theorems.

;(defthm integer->bytes->integer-thm
;  (implies (n-bytep temp-int bytes)
;           (equal temp-int (bytes->integer (integer->bytes temp-int bytes)))))

; (integer->bytes-thm
; Tests that integer->bytes returns a list for any lst and number input.
(defthm integer->bytes-thm
  (true-listp (integer->bytes lst n)))

; (shift-8-bits-lemma)
; If lst is a list then shifting it 8 bits returns a natural number list.
(defthm shift-8-bits-lemma
  (implies (true-listp lst) (nat-listp (shift-8-bits lst))))

;; DoubleCheck

; (2scomp-test)
; If we convert something to 2scomp number and convert it back, it should
; be equal.
(defproperty 2scomp-test 20
  ((bits (random-int-between 4 32)
         t)
   (num (random-int-between 0 (expt 2 bits))
        t))
  (equal num (2scomp->unsigned (unsigned->2scomp num bits) bits)))

; (intbytes-test)
; If we convert a number to a bytelist and then back to a number, it should
; be equal.
(defproperty intbytes-test 20
  ((bytes (random-int-between 2 16)
         t)
   (num (random-int-between 0 (expt 2 (- (* 8 bytes) 1)))
        t))
  (equal num (bytes->integer (integer->bytes num bytes))))

; (add-lists-test)
; Tests that add-lists returns a list with length max(len(one), len(two)).
; Addresses bugs #6 and #7.
(defproperty add-lists-test 20
  ((list1 (random-list-of (random-int-between -500 500)
                          (random-int-between 0 500))
          (listp list1))
   (list2 (random-list-of (random-int-between -500 500)
                          (random-int-between 0 500))
          (listp list1)))
  (equal (max (length list1) (length list2)) (length (add-lists list1 list2))))

; (bytes->integer-8bit-scale)
; Tests that bytes->integer on an 8 bit is within a certain range.
; Addresses bugs #14.
(defproperty bytes->integer-8bit-scale 20
  ((byte (random-list (random-int-between 0 255)) (integer-listp byte)))
  (and (> (bytes->integer byte) -129)
          (< (bytes->integer byte) 128)))

; (random-normalized-test power)
; Generates a random normalized rational number between 0 and 2^(power-1)
(defgenerator random-normalized-rat (power)
  (1 (/ (random-int-between 0 (- (expt 2 power) 1)) (- (expt 2 power) 1))))

(defun wav->list (wav)
  (list (wav-file-chunk-id wav) ;chunk-id
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
        (wav-file-subchunk-2-size wav)
        (wav-file-data wav)))

(defun list->wav (lst)
  (wav-file (nth 0 lst)
            (nth 1 lst)
            (nth 2 lst)
            (nth 3 lst)
            (nth 4 lst)
            (nth 5 lst)
            (nth 6 lst)
            (nth 7 lst)
            (nth 8 lst)
            (nth 9 lst)
            (nth 10 lst)
            (nth 11 lst)
            (nth 12 lst)
            (nth 13 lst)))


(defgenerator wav-gen (max-length)
  (1 (random-list -1
                  2684
                  -1
                  -1
                  16
                  1
                  (random-int-between 1 2)
                  (random-int-between 8000 96000)
                  -1
                  -1
                  (* (random-int-between 1 4) 8)
                  -1
                  -1
                  (random-list-of (random-normalized-rat 16) (random-int-between 0 max-length)))))

(defproperty wav-test 1
  ((wav (wav-gen 1000) t))
  (equal wav (wav->list (boost 1 (list->wav wav)))))

;filter tests

; (padding-test)
; Makes sure the length of (padding lst) is half of lst.
(defproperty padding-test 20
  ((lst (random-list-of (random-int-between 0 1)
                        (random-int-between 1 50))
        t))
  (equal (len (padding lst)) (floor (len lst) 2)))

; (validate-filter-length-test)
; Verifies that validate-filter-length returns an odd filter length
(defproperty validate-filter-length-test 20
  ((lst (random-list-of (random-int-between 0 1)
                        (random-int-between 1 200))
        t)
   (filter (validate-filter-length lst)
           t))
  (oddp (len filter)))

(check-properties)
