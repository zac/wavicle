(in-package "ACL2")
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

;---------------------- ECHO -------------------------

(defun multiply-all (val l)
  (if (endp l)
      nil
      (cons (* val (car l)) (multiply-all val (cdr l)))))

(defun add-lists (first second)
  (if (or (endp first)
          (endp second))
      nil
      (cons (+ (car first) (car second)) (add-lists (cdr first) (cdr second)))))

(defun firstn (n l)
  (if (or (endp l)
          (zp n))
      nil
      (cons (car l) (firstn (- n 1) (cdr l)))))

;(defthm nthcdr-thm
;  (implies (and (listp list1)
;                (listp list2)
;                (not (endp list1))
;                (not (endp list2)))
;           (> (length list2) (length (nthcdr (length list1) list2)))))

(defthm multiply-all-thm
  (implies (and (listp list1)
                (rationalp val))
           (= (length list1) (length (multiply-all val list1)))))

;(defun overdub-repeat (slice val samples)
;  (if (or (endp slice)
;          (endp samples))
;      samples
;      (let ((scaled (multiply-all val slice))
;            (firsts (firstn (length slice) samples)))
;        (append (add-lists scaled firsts) (overdub-repeat scaled val (nthcdr (length slice) samples))))))
  
;(defun echo-h (num-samples val samples)
;  (if (endp samples)
;      nil
;      (let ((current (firstn num-samples samples)))
;        (append current (echo-h num-samples val (overdub-repeat current val (nthcdr num-samples samples)))))))

;(defun echo (t val wav)
;  (modify-data wav (echo-h (floor (* (* (wav-file-sample-rate wav) (wav-file-num-channels wav)) t) 1) val ;(wav-file-data wav))))


;--------------------- OVERDUB -----------------------
;overdubs wav1 onto wav2.
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

(defun cut (time wav)
  (let ((num-samples (floor (* time (wav-file-sample-rate wav)) 1))
        (data (wav-file-data wav)))
    (modify-data wav (nthcdr num-samples (butlast data num-samples)))))

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

;--------------------- FILTER ------------------------
(defun generate-boundary (n)
  (if (zp n)
      nil
      (cons 0 (generate-boundary (- n 1)))))

;ys is the filter list
(defun padding (ys)
  (generate-boundary (floor (len ys) 2)))

(defun get-first-part (xs)
  (take (floor (len xs) 2) 
        xs))

(defun get-rest (xs)
  (nthcdr (floor (len xs) 2) xs))

(defun sum-elements (xs ys)
  (if (consp xs)
      (+ (* (car xs) (car ys))
         (sum-elements (cdr xs) (cdr ys)))
      0))

(defun compile-filter (xs ys)
  (let ((center (sum-elements xs ys))
        (front (get-first-part xs))
        (rest (cdr (get-rest xs))))
    (append front (cons center rest))))

(defun get-modifiable-list (xs ys)
  (take (len ys) xs))

(defun get-rest-of-list (xs ys)
  (nthcdr (len ys) xs))

(defun filter-helper (xs ys n)
  (if (zp n)
      xs
      (let ((modified (compile-filter (get-modifiable-list xs ys) ys))
            (rest (get-rest-of-list xs ys)))
        (cons (car modified) 
              (filter-helper (append 
                              (cdr modified) rest) ys (- n 1))))))

(defun filter-handler (xs ys)
  (let* ((p (padding ys))
         (results (filter-helper (append p (append xs p))
                                 ys 
                                 (len xs)))
         (n (- (len results) (len p)))
         )
    ;remove padding
    (nthcdr (len p) (take n results))))
