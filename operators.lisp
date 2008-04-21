(in-package "ACL2")
;(include-book "structures")

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
            (* (length new-data) (* (wav-file-num-channels wav) (/ (wav-file-bits-per-sample wav) 8)))
            new-data))

(defun modify-sample-rate (wav new-rate)
  (wav-file (wav-file-chunk-id wav) ;chunk-id
            (wav-file-chunk-size wav) ;chunk-size
            (wav-file-format wav) ;format
            (wav-file-subchunk-1-id wav) ;subchunk-1-id
            (wav-file-subchunk-1-size wav) ;subchunk-1-size
            (wav-file-audio-format wav) ;audio-format
            (wav-file-num-channels wav) ;num-channels
            new-rate
            (wav-file-byte-rate wav) ;byte-rate
            (wav-file-block-align wav) ;block-align
            (wav-file-bits-per-sample wav) ;bits-per-sample
            (wav-file-subchunk-2-id wav) ;subchunk-2-id
            (wav-file-subchunk-2-size wav) ;subchunk-2-size
            (wav-file-data wav)))

;--------------------UTILITIES-----------------------
;shortens the list
(defun shorten-list (xs n count)
  (if (> (len xs) n)
      (if (consp xs)
          (if (= count n)
              (cons (car xs) (shorten-list (cdr xs) n 0))
              (shorten-list (cdr xs) n (+ count 1)))
          nil)
      xs))

;---------------------- BOOST ------------------------

(defun boost-h (b samples)
  (if (endp samples)
      nil
      (cons (* (car samples) b) (boost-h b (cdr samples)))))

(defun boost (b wav)
  (modify-data wav (boost-h b (wav-file-data wav))))


;----------------------- FUZZ -------------------------

; (maximum list)
;   Gives the maximum element of the list.
;   list = list of numbers to compare.
; (maximum '(1 2 43 54 23 11 32 55 33)) -> 55
(defun maximum (list)
  (if (consp (cdr list))
      (max (car list) (maximum (cdr list)))
      (car list)))

(defun fuzz-h (h g data)
  (if (endp data)
      nil
      (let ((f (* g h)))
        (cons (max (- f) (min f (car data))) (fuzz-h h g (cdr data))))))

(defun fuzz (val wav)
  (let* ((data (wav-file-data wav))
         (g (maximum data)))
    (modify-data wav (fuzz-h val g (wav-file-data wav)))))

;---------------------- DELAY ------------------------

(defun gen-0-list (num)
  (if (posp num)
      (cons 0 (gen-0-list (- num 1)))
      nil))

(defun delay (time wav)
  (let* ((sample-rate (wav-file-sample-rate wav))
         (num-packets (* (* sample-rate (wav-file-num-channels wav)) time))
         (data (wav-file-data wav)))
    (modify-data wav (append (gen-0-list num-packets) data))))


;--------------------- OVERDUB -----------------------
;overdubs wav1 onto wav2.
(defun add-lists (first second)
  (if (endp first)
      second
      (if (endp second)
          first
          (cons (+ (car first) (car second)) (add-lists (cdr first) (cdr second))))))

(defun overdub (wav1 wav2)
  (modify-data wav2 (add-lists (wav-file-data wav1) (wav-file-data wav2))))

;--------------------- FADE-IN -----------------------

(defun fade-in-h (total samples)
  (if (endp samples)
      nil
      (cons (* (/ (- total (length samples)) total) (car samples)) (fade-in-h total (cdr samples)))))

(defun fade-in (time wav)
  (let* ((sample-rate (wav-file-sample-rate wav))
         (num-packets (* (* sample-rate (wav-file-num-channels wav)) time))
         (data (wav-file-data wav)))
    (modify-data wav (append (fade-in-h num-packets
                                        (butlast data (- (length data) num-packets)))
                             (nthcdr num-packets data)))))

;---------------------- ECHO -------------------------

(defun multiply-list (val xs)
  (declare (xargs :guard (and (true-listp xs) (rationalp val))
                  :verify-guards t))
  (if (consp xs)
      (cons (* (car xs) val)
            (multiply-list val (cdr xs)))
      nil))

(defun get-slice (x length xs) 
  (multiply-list x (take length xs)))

;ys is the data
(defun add-lists-echo (xs ys)
  (if (and (consp xs)
           (consp ys))
      (cons (+ (car xs) (car ys))
            (add-lists-echo (cdr xs) (cdr ys)))
      ys))

(defun echo-helper (slice val xs n)
  (if (zp n)
      nil
      (let* ((length (if (> (len xs) (len slice))
                         (len slice)
                         (len xs)))
             (modifiable-part (take length xs))
             (rest (nthcdr length xs))
             (new-slice (multiply-list val modifiable-part))
            )
        (append (add-lists-echo slice modifiable-part)
                (echo-helper new-slice
                      val
                      rest
                      (- n 1))))))
                

(defun echo-handler (length val xs)
  (let ((slice (get-slice val length xs))
        (rest (nthcdr length xs))
        (run-time (floor (len xs) length)))
    (append slice
            (echo-helper slice
                         val
                         rest
                         run-time))))

(defun echo (time val wav)
  (modify-data wav (echo-handler (floor (* (* (wav-file-sample-rate wav) (wav-file-num-channels wav)) time) 1) val (wav-file-data wav))))

;--------------------- FADE-OUT ----------------------

(defun fade-out-h (total samples)
  (if (endp samples)
      nil
      (cons (* (/ (length samples) total) (car samples)) (fade-out-h total (cdr samples)))))

(defun fade-out (time wav)
  (let* ((sample-rate (wav-file-sample-rate wav))
         (num-packets (* (* sample-rate (wav-file-num-channels wav)) time))
         (data (wav-file-data wav)))
    (modify-data wav (append (butlast data num-packets)
                             (fade-out-h num-packets
                                         (nthcdr (- (length data) num-packets) data))))))

;----------------------- CUT -------------------------

(defun cut (begin end wav)
  (let ((begin-num-samples (floor (* begin (wav-file-sample-rate wav)) 1))
        (end-num-samples (floor (* end (wav-file-sample-rate wav)) 1))
        (data (wav-file-data wav)))
    (modify-data wav (nthcdr begin-num-samples (butlast data end-num-samples)))))

;--------------------- CHIPMUNK -----------------------
(defun chipmunk (p wav)
  (if (< p 0)
      wav
      (modify-sample-rate wav (floor (* p (wav-file-sample-rate wav)) 1))))

;--------------------- REVERSE ------------------------
(defun audio-reverse (wav)
  (modify-data wav (reverse (wav-file-data wav))))

(defun normalize-data (data maximum)
  (if (endp data)
      nil
      (cons (/ (car data) maximum) (normalize-data (cdr data) maximum))))
