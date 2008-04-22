(in-package "ACL2")

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
            (* (length new-data) (* (wav-file-num-channels wav) 
                                    (/ (wav-file-bits-per-sample wav) 8)))
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
            (* new-rate (* (wav-file-num-channels wav)
                           (/ (wav-file-bits-per-sample wav) 8))) ;byte-rate
            (wav-file-block-align wav) ;block-align
            (wav-file-bits-per-sample wav) ;bits-per-sample
            (wav-file-subchunk-2-id wav) ;subchunk-2-id
            (wav-file-subchunk-2-size wav) ;subchunk-2-size
            (wav-file-data wav)))

;--------------------UTILITIES-----------------------
; (shorten-list)
; Shortens a list
;
; xs - The list to be shortened.
; n - The number of elements to skip.
; count - Temporary counter.
(defun shorten-list (xs n count)
  (if (> (len xs) n)
      (if (consp xs)
          (if (= count n)
              (cons (car xs) (shorten-list (cdr xs) n 0))
              (shorten-list (cdr xs) n (+ count 1)))
          nil)
      xs))

;---------------------- BOOST ------------------------
; (boost-h b samples)
; Helper for boost.  Applies boost to each sample.
;
; b - The boost parameter from (boost b wav).
; samples - The data from wav in (boost b wav).
(defun boost-h (b samples)
  (if (endp samples)
      nil
      (cons (* (car samples) b) (boost-h b (cdr samples)))))

; (boost b wav)
; Boosts a wav file by multiplying all amplitudes in the data
; by a constant.
;
; b - The boost parameter to multiply ampltudes by.
; wav - The structure that represents the wav file to be modified.
(defun boost (b wav)
  (modify-data wav (boost-h b (wav-file-data wav))))


;----------------------- FUZZ -------------------------

; (maximum list)
; Finds the maximum element in a list.
;
; list - The list that contains the maximum to be found.
(defun maximum (list)
  (if (consp (cdr list))
      (max (car list) (maximum (cdr list)))
      (car list)))

; (fuzz-h h g data)
; Performs the followiing to cause a "fuzz effect on data:
; max(-f, min(f, x(c, t)), where f = h*g
;
; g - Amplitude of greatest magnitude in the wav data.
; h - The parameter passed from fuzz (ranges from 0 to 1).
; data - The data from the wav file in (fuzz val wav).
(defun fuzz-h (h g data)
  (if (endp data)
      nil
      (let ((f (* g h)))
        (cons (max (- f) (min f (car data))) (fuzz-h h g (cdr data))))))

; (fuzz val wav)
; Modifies the data in a wav file fith similar data that is "fuzzier."
;
; val - The parameter to be used in the fuzz algorithm.
; wav - The structure that represents the wav file to be modified.
(defun fuzz (val wav)
  (let* ((data (wav-file-data wav))
         (g (maximum data)))
    (modify-data wav (fuzz-h val g (wav-file-data wav)))))

;---------------------- DELAY ------------------------
; (gen-0-list num)
; Creates a list of a specified length that consists of all zeroes.
;
; num - The length of the list
(defun gen-0-list (num)
  (if (posp num)
      (cons 0 (gen-0-list (- num 1)))
      nil))

; (delay time wav)
; Delays a wav file's audio by a specified amount of time.
;
; time - The amount of time to delay the audio in seconds.
; wav - The structure that represents the wav file to be modified.
(defun delay (time wav)
  (let* ((sample-rate (wav-file-sample-rate wav))
         (num-packets (* (* sample-rate (wav-file-num-channels wav)) time))
         (data (wav-file-data wav)))
    (modify-data wav (append (gen-0-list num-packets) data))))


;--------------------- OVERDUB -----------------------
; (add-lists first second)
; Adds all of the elements from one list to another list.
;
; first - The first list.
; second - The second list.
(defun add-lists (first second)
  (if (endp first)
      second
      (if (endp second)
          first
          (cons (+ (car first) (car second)) 
                (add-lists (cdr first) (cdr second))))))

; (overdub wav1 wav2)
; Modifies the data in wav2 to be an overdub of wav1 and wav2.
;
; wav1 - The structure that represents the first wav file.
; wav2 - The structure that represents the second wav file.
(defun overdub (wav1 wav2)
  (modify-data wav2 (add-lists (wav-file-data wav1) (wav-file-data wav2))))

;--------------------- FADE-IN -----------------------
; (fade-in-h total samples)
; Takes a list of samples and modifies each element to be a fraction
; of each element's original value.
;
; total - The total number of samples to be faded-in.
; samples - The audio data to be faded-in.
(defun fade-in-h (total samples)
  (if (endp samples)
      nil
      (cons (* (/ (- total (length samples)) total) (car samples))
            (fade-in-h total (cdr samples)))))

; (fade-in time wav)
; Modifies the audio in a wav file to simulate a fade-in effect
; from 0 to 100% over a specified amount of time.
;
; time - The amount of time to fade-in in seconds.
; wav - The structure that represents the wav file to be modified.
(defun fade-in (time wav)
  (let* ((sample-rate (wav-file-sample-rate wav))
         (num-packets (* (* sample-rate (wav-file-num-channels wav)) time))
         (data (wav-file-data wav)))
    (modify-data wav 
                 (append (fade-in-h num-packets
                                    (butlast data 
                                             (- (length data) num-packets)))
                         (nthcdr num-packets data)))))

;---------------------- ECHO -------------------------

; (multiply-list val xs)
; Function takes a rational value (val) and a list (xs) and returns a list
; where each element in xs is multiplied by val.
; val = a rational value to multiply with each element in a list.
; xs = a list of rational numbers
(defun multiply-list (val xs)
  (declare (xargs :guard (and (true-listp xs) (rationalp val))
                  :verify-guards t))
  (if (consp xs)
      (cons (* (car xs) val)
            (multiply-list val (cdr xs)))
      nil))

; (get-slice x length xs)
; Function takes a value (x), the length of the slice (length), and a list of 
; rational numbers (xs).  Then it gets the first length samples from xs and 
; applies (multiply-list x xs) to the slice.
; x = the scaling factor.
; length = the length of the slice to return.
; xs = a list of rational numbers.
(defun get-slice (x length xs) 
  (multiply-list x (take length xs)))

; (add-lists-echo xs ys)
; Function takes two lists, one the previous scaled time data (xs) and the 
; current time slice (ys) and addes, or overdubs, the two two.  If the lists
; are not the same size, the rest of the original time data is consed onto
; the return list.
; xs = the scaled previous data
; ys = the data
(defun add-lists-echo (xs ys)
  (if (and (consp xs)
           (consp ys))
      (cons (+ (car xs) (car ys))
            (add-lists-echo (cdr xs) (cdr ys)))
      ys))

; (echo-helper slice val xs n)
; Function does the main work to apply echo.  It gets new time slices,
; applies the scaling factor to those, and overdubs the echo effect onto
; future data.
; slice = The current time slice to have echoed in xs.
; val = The value to scale the slice by.
; xs = The data samples that can have the slice overdubed onto them.
; n = This is a counter applied to the function to help prove it terminates.
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
                
; (echo-handler length val xs)
; Function handles the basic operations of applying echoing a wav structure.
; length = The number of samples to echo, basically the time to echo
; the wav data by.
; val = The value to scale the echo samles by.
; xs = The data from the wav structure.
(defun echo-handler (length val xs)
  (if (zp length)
      xs
      (let ((slice (get-slice val length xs))
            (rest (nthcdr length xs))
            (run-time (floor (len xs) length)))
        (append slice
                (echo-helper slice
                             val
                             rest
                             run-time)))))

; (echo time val wav)
; Function creates an echo effect on a wav structure by overdubing a 
; current time of samples onto a later time of samples.
; time = Number of seconds to echo.
; val = The rational value to scale the current time by, thus making the
; sound appear to be fading out later.
; wav = The wav structure to apply echo to.
(defun echo (time val wav)
  (modify-data wav 
               (echo-handler 
                ;calculate the number of samples in the time frame, flooring
                ;because it must be an integer
                    (floor (* (* (wav-file-sample-rate wav) 
                                 (wav-file-num-channels wav)) time) 1) 
                    val (wav-file-data wav))))

;--------------------- FADE-OUT ----------------------
; (fade-out-h total samples)
; Takes a list of samples and modifies each element to be a fraction
; of each element's original value.
;
; total - The total number of samples to be faded-out.
; samples - The audio data to be faded-out.
(defun fade-out-h (total samples)
  (if (endp samples)
      nil
      (cons (* (/ (length samples) total) (car samples))
            (fade-out-h total (cdr samples)))))

; (fade-out time wav)
; Modifies the audio in a wav file to simulate a fade-out effect
; from 100 to 0% over a specified amount of time.
;
; time - The amount of time to fade out at the end of the file in seconds.
; wav - The structure that represents the wav file to be modified.
(defun fade-out (time wav)
  (let* ((sample-rate (wav-file-sample-rate wav))
         (num-packets (* (* sample-rate (wav-file-num-channels wav)) time))
         (data (wav-file-data wav)))
    (modify-data wav (append (butlast data num-packets)
                             (fade-out-h num-packets
                                         (nthcdr (- (length data) num-packets)
                                                 data))))))

;----------------------- CUT -------------------------
; (cut begin end wav)
; Modifies the audio in a wav file to have a cut of specified amounts of
; time from the beginning and end of the file.
;
; begin - The amount of time to cut from the beginning in seconds.
; end - The amount of time to cut from the end in seconds.
(defun cut (begin end wav)
  (let ((begin-num-samples (floor (* begin (wav-file-sample-rate wav)) 1))
        (end-num-samples (floor (* end (wav-file-sample-rate wav)) 1))
        (data (wav-file-data wav)))
    (modify-data wav (nthcdr begin-num-samples 
                             (butlast data end-num-samples)))))

;--------------------- CHIPMUNK -----------------------
; (chipmunk p wav)
; Changes the speed at which an audio files plays back.
;
; p - The multiplier for the speed of the audio file.
; wav - The structure that represents the wav file to be modified.
(defun chipmunk (p wav)
  (if (< p 0)
      wav
      (modify-sample-rate wav (floor (* p (wav-file-sample-rate wav)) 1))))

;--------------------- REVERSE ------------------------
; (audio-reverse wav)
; Reverses the audio data of a wav file.
;
; wav - The structure that represents the wav file to be modified.
(defun audio-reverse (wav)
  (modify-data wav (reverse (wav-file-data wav))))

(defun normalize-data (data maximum)
  (if (endp data)
      nil
      (cons (/ (car data) maximum) (normalize-data (cdr data) maximum))))