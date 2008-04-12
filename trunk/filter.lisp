(in-package "ACL2")
(include-book "io-utilities" :dir :teachpacks)
(include-book "list-utilities" :dir :teachpacks)
(include-book "operators")

(set-state-ok t)

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

(defun get-modifiable-list (xs ys)
  (take (len ys) xs))

(defun filter-helper (xs ys n)
  (if (zp n)
      nil
      (cons (sum-elements (get-modifiable-list xs ys) ys)
            (filter-helper (cdr xs) ys (- n 1)))))

;(defun filter-helper (xs ys n)
;  (if (zp n)
;      xs
;      (let ((modified (compile-filter (get-modifiable-list xs ys) ys))
;            (rest (get-rest-of-list xs ys)))
;        (cons (car modified) 
;              (filter-helper (append 
;                              (cdr modified) rest) ys (- n 1))))))

(defun filter-handler (xs ys)
  (let* ((p (padding ys))
         (newData (append p (append xs p)))
         (n (- (- (len newData) (len p)) (len p)))
         (results (filter-helper newData ys n))
         )
    results))

;get filter code
(defun validate-filter-length (xs)
  (if (= (mod (len xs) 2) 0)
      (cons 0 xs)
      xs))
  
(defun parse-filter (xs)
  (if (consp xs)     
      (let ((number (str->rat (chrs->str (car xs)))))
        (if (> number 1)
            (cons 1 (parse-filter (cdr xs)))
            (if (< number 0)
                (cons 0 (parse-filter (cdr xs)))
                (cons number (parse-filter (cdr xs))))))
      nil))
  
(defun read-filter (file state)
  (mv-let (line error state)
          (file->string file state)
          (if error
              (mv error state)
              (mv (validate-filter-length (parse-filter (packets #\, (str->chrs line)))) state))))

;end of get filter

(defun filter (ys wav)
  (modify-data wav (filter-handler (wav-file-data wav) ys)))