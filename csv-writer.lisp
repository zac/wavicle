(in-package "ACL2")
;(include-book "io-utilities" :dir :teachpacks)
;(include-book "world" :dir :teachpacks)
;(include-book "list-utilities" :dir :teachpacks)

#|---------------------Convert Rational numbers to string----------------------|#
;converts a list of rational numbers to a list of comma seperated strings
(defun convert-to-string (str)
  (if (consp str)
      (concatenate 'string (rat->str (car str) 5) 
                   (concatenate 'string "," (convert-to-string (cdr str))))
      ""))

;removes the last comma in the list of strings
(defun remove-last-comma (str)
  (subseq str 0 (- (length str) 1)))

;shortens the list
(defun shorten-list (xs n count)
  (if (> (len xs) n)
      (if (consp xs)
          (if (= count n)
              (cons (car xs) (shorten-list (cdr xs) n 0))
              (shorten-list (cdr xs) n (+ count 1)))
          nil)
      xs))

;combine the two above functions.
(defun get-csv-list (ratlist)
  (let* ((n (* (floor (/ (len ratlist) 1000) 2)))
         (str-comma (convert-to-string (shorten-list ratlist n 0)))
         (str       (remove-last-comma str-comma)))
    str))
#|--------------------------------End of this code----------------------------|#


#|------------------------Writing the CSV file--------------------------------|#
(defun write-csv (file str state)
  (mv-let (error state)
          (string-list->file file (list (get-csv-list str)) state)
          (if error
              (mv error state)
              (mv "Success" state))))
#|--------------------------------End of this code----------------------------|#