(in-package "ACL2")

(defun multiply-list (val xs)
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
