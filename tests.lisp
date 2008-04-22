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

; (wav->list wav)
; Converts a wav structure to a list so that DoubleCheck can handle it.
; wav = the input wav to be converted.
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

; (list->wav lst)
; Converts a list representing a wav structure into a wav-file structure.
; lst = the list to be converted.
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

; (random-wav min-length max-length)
; A generator to randomly make a random list representing a wav-file structure.
; min-length = minimum data length for the wav-file.
; max-length = maximum data length for the wav-file.
(defgenerator random-wav (min-length max-length)
  (1 (random-list (random-int-between 0 0)
                  (random-int-between 0 0)
                  (random-int-between 0 0)
                  (random-int-between 0 0)
                  (random-int-between 0 0)
                  (random-int-between 0 0)
                  (random-int-between 1 2)
                  (random-int-between 8000 44000)
                  (random-int-between 0 0)
                  (random-int-between 0 0)
                  (random-int-between 1 4)
                  (random-int-between 0 0)
                  (random-int-between 0 0)
                  (random-list-of (random-int-between -128 127) (random-int-between min-length max-length)))))

; (data-length wav)
; Convenience function for accessing data length.
; wav = the wav list to access length of.
(defun data-length (wav)
  (length (nth 13 wav)))

; (data-equal (wav1 wav2)
; Convenience function for checking data equal.
; wav1 = first wav to compare.
; wav2 = second wav to compare.
(defun data-equal (wav1 wav2)
  (equal (nth 13 wav1) (nth 13 wav2)))

; (boost-1-equal)
; Boosts a wav once and checks to see if it is the same.
(defproperty boost-1-equal 5
  ((wav (random-wav 100 10000) t))
  (data-equal wav (wav->list (boost 1 (list->wav wav)))))

; (fade-in-0-equal)
; Fades in a wav for 0 seconds and checks if it stays the same.
(defproperty fade-in-0-equal 5
  ((wav (random-wav 100 10000) t))
  (data-equal wav (wav->list (fade-in 0 (list->wav wav)))))

; (fade-out-0-equal)
; Fades out a wav for 0 seconds and checks if it stays the same.
(defproperty fade-out-0-equal 5
  ((wav (random-wav 100 10000) t))
  (data-equal wav (wav->list (fade-out 0 (list->wav wav)))))

; (delay-0-equal)
; Delays a wav for 0 seconds and checks if it stays the same.
(defproperty delay-0-equal 5
  ((wav (random-wav 100 10000) t))
  (data-equal wav (wav->list (delay 0 (list->wav wav)))))

; (cut-0-0-equal)
; Cuts 0 from the back and front and checks if it stays the same.
(defproperty cut-0-0-equal 5
  ((wav (random-wav 100 10000) t))
  (data-equal wav (wav->list (cut 0 0 (list->wav wav)))))

; (echo-0-equal)
; Echos for 0 seconds and checks if it stays the same.
(defproperty echo-0-equal 5
  ((wav (random-wav 100 10000) t)
   (mult (random-int-between 1 2) t))
  (data-equal wav (wav->list (echo 0 mult (list->wav wav)))))

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

; Runs DoubleCheck.
(check-properties)
