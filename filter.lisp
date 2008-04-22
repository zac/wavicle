(in-package "ACL2")

(include-book "io-utilities" :dir :teachpacks)

;====================filter processing code====================

; (generate-boundary xs)
; This function creates the boundary needed to pad the egdes of a wav
; data section.
; n = The number of 0's to put in the output
(defun generate-boundary (n)
  (if (zp n)
      nil
      (cons 0 (generate-boundary (- n 1)))))

; (padding ys)
; This function creates the padding required for the filter to run by
; taking in the filter list, flooring it by 2 and generating the
; boundary.  It only needs to be half the length of the filter since
; the filter modifies the center element.
; ys = the filter list
(defun padding (ys)
  (generate-boundary (floor (len ys) 2)))

; (sum-elements xs ys)
; This function takes two lists and takes each element from the first
; list and multiplies it with the adjacent indexed element in the 
; second list.  All of those elements are summed together and a rational
; number is returned.
; xs = the first list
; ys = the second list
(defun sum-elements (xs ys)
  (if (consp xs)
      (+ (* (car xs) (car ys))
         (sum-elements (cdr xs) (cdr ys)))
      0))

; (get-modifiable-list xs ys)
; This function takes in the filter (ys) and the wav data (xs), and 
; returns the modifiable part of the wav data.
; xs = the wav data list
; ys = the filter list
(defun get-modifiable-list (xs ys)
  (take (len ys) xs))

; (filter-helper xs ys n)
; The filter helper takes a counter (n) and the wav data (xs)
; and the filter list (ys).  The filter is then run as described
; until the counter reaches 0.
; xs = the wav data list
; ys = the filter list
; n = the counter
(defun filter-helper (xs ys n)
  (if (zp n)
      nil
      (cons (sum-elements (get-modifiable-list xs ys) ys)
            (filter-helper (cdr xs) ys (- n 1)))))

; (filter-handler xs ys)
; The filter-handler takes in a wav data (xs) and a filter list (ys)
; and handles the filtering process
; xs = the wav data list
; ys = the filter list
(defun filter-handler (xs ys)
  (let* ((p (padding ys))
         (newData (append p (append xs p)))
         (n (- (- (len newData) (len p)) (len p)))
         (results (filter-helper newData ys n))
         )
    results))

;====================filter io code====================

; (validate-filter-length xs)
; Function validates a filter to make sure that it is
; of length 2k + 1, i.e. the length is an odd number
; xs = a filter list
(defun validate-filter-length (xs)
  (if (= (mod (len xs) 2) 0)
      (cons 0 xs)
      xs))
  
; (parse-filter xs)
; Function parses the filter file and creates a list of rationals.
; It also ensures that each filter number is between 1 and -1
; xs = the packets returned from read-filter.
(defun parse-filter (xs)
  (if (consp xs)     
      (let ((number (str->rat (chrs->str (car xs)))))
        (if (> number 1)
            (cons 1 (parse-filter (cdr xs)))
            (if (< number -1)
                (cons -1 (parse-filter (cdr xs)))
                (cons number (parse-filter (cdr xs))))))
      nil))
  
; (read-filter file state)
; Function reads in a filter file and sends it to the parsing
; helpers.  
; file = the file path of the filter file
; state = the state
(defun read-filter (file state)
  (mv-let (line error state)
          (file->string file state)
          (if error
              (mv error state)
              (mv (validate-filter-length 
                   (parse-filter (packets #\, (str->chrs line)))) state))))

;====================end filter io code====================

; Fucntion is the top level function for filtering a wav structure.
; ys = a filter list
; wav = a wav structure
(defun filter (ys wav)
  (modify-data wav (filter-handler (wav-file-data wav) ys)))