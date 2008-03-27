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