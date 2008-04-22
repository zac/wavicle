(in-package "ACL2")
(include-book "doublecheck" :dir :teachpacks)
(include-book "structures")
(include-book "operators")
(include-book "wav")
(include-book "filter")

;; Theorems.
(defthm bytes->integer-thm
  (<= 0 (bytes->integer byte-list)))

(defthm integer->bytes->integer-thm
  (implies (n-bytep temp-int bytes)
           (equal temp-int (bytes->integer (integer->bytes temp-int bytes)))))

(defthm integer->bytes-thm
  (true-listp (integer->bytes lst n)))

(defthm shift-8-bits-lemma
  (implies (true-listp lst) (nat-listp (shift-8-bits lst))))

;; DoubleCheck
(defproperty 2scomp-test 20
  ((bits (random-int-between 4 32)
         t)
   (num (random-int-between 0 (expt 2 bits))
        t))
  (equal num (2scomp->unsigned (unsigned->2scomp num bits) bits)))

(defproperty intbytes-test 20
  ((bytes (random-int-between 2 16)
         t)
   (num (random-int-between 0 (expt 2 (- (* 8 bytes) 1)))
        t))
  (equal num (bytes->integer (integer->bytes num bytes))))

;; For bugs #6 and #7
(defproperty add-lists-test 20
  ((list1 (random-list-of (random-int-between -500 500) (random-int-between 0 500))
          (listp list1))
   (list2 (random-list-of (random-int-between -500 500) (random-int-between 0 500))
          (listp list1)))
  (equal (max (length list1) (length list2)) (length (add-lists list1 list2))))

;; For bug #14
(defproperty bytes->integer-8bit-scale 20
  ((byte (random-list (random-int-between 0 255)) (integer-listp byte)))
  (and (> (bytes->integer byte) -129)
          (< (bytes->integer byte) 128)))

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

(defun generate-wav-list (num-channels sample-rate bits-per-sample num-samples random-lst)
  (list '(82 73 70 70)
        2684
        '(87 65 86 69)
        '(102 109 116 32)
        16
        1
        num-channels
        sample-rate
        (* sample-rate num-channels bits-per-sample)
        (* num-channels (/ bits-per-sample 8))
        bits-per-sample
        '(100 97 116 97)
        (* (* num-samples num-channels) (/ bits-per-sample 8))
        random-lst))
        

;(defgenerator wav-gen (max-length)
;  (1 (generate-wav-list (random-int-between 1 2)
;                        (random-int-between 8000 96000)
;                        (* 8 (random-int-between 1 4))
;                        (random-int-between 10 max-length)
;                        (random-list-of (random-normalized-rat (- bits-per-sample 1)) data-length))))

;(defproperty wav-test 1
;  ((wav (wav-gen 1000) t))
;  (equal wav (wav->list (boost 1 (list->wav wav)))))

;filter tests
(defproperty padding-test 20
  ((lst (random-list-of (random-int-between 0 1)
                        (random-int-between 1 50))
        t))
  (equal (len (padding lst)) (floor (len lst) 2)))

(defproperty validate-filter-length-test 20
  ((lst (random-list-of (random-int-between 0 1)
                        (random-int-between 1 200))
        t)
   (filter (validate-filter-length lst)
           t))
  (equal (mod (len filter) 2) 1))

(check-properties)
