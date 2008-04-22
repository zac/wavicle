(in-package "ACL2")

; (convert-to-string str)
; Function converts a list of rational numbers into a string where
; each rational is separated by a comma.  The last rational will 
; have a comma after it, that needs replacing by remove-last-comma.
; str = the list of rationals being passed into the function.
(defun convert-to-string (str)
  (if (consp str)
      (concatenate 'string (rat->str (car str) 5) 
                   (concatenate 'string "," (convert-to-string (cdr str))))
      ""))

; (remove-last-comma str)
; Function removes the last character in a string passed to it, which
; for our use will remove the last comma generated from the convert-to-string
; function.
; str = The string needing the last character removed.
(defun remove-last-comma (str)
  (subseq str 0 (- (length str) 1)))

; (get-csv-list ratlist)
; Wrapper function that takes in a rational list and returns a string
; where each rational number is separated by a comma.  This function strips
; down the samples so that there are only around 1000 samples left.  This
; helps reduce the total time it takes to write the file.  The output will
; still appear similar since there are still many samples.
; ratlist = the list of rationals from the data section of the wav file.
(defun get-csv-list (ratlist)
  (let* ((n (* (floor (len ratlist) 2000) 2))
         (str-comma (convert-to-string (shorten-list ratlist n 0)))
         (str       (remove-last-comma str-comma)))
    str))

; (write-csv file str state)
; The function that manages writing a rational list to a csv file.  If there 
; is an error it outputs the error, otherwise outputs Success.
; file = the path to write the file.
; str = the list of rational numbers needing concatenated into a csv string.
; state = the state required by acl2.
(defun write-csv (file str state)
  (mv-let (error state)
          (string-list->file file (list (get-csv-list str)) state)
          (if error
              (mv error state)
              (mv "Success" state))))
