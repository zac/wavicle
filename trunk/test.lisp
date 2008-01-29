;(in-package "ACL2")

(include-book "lib/data-structures/utilities")

(include-book "lib/ihs/ihs-definitions")
(include-book "lib/ihs/ihs-lemmas")

(defun logapp-test1 ()
  (logapp 16 10 124))

;(defun bytes->integer-h (bytes size)
;  (logapp size (bytes->integer-h (cdr bytes) size) (car bytes)))

;(defun bytes->integer (bytes)
;  (bytes->integer-h bytes (* 8 (length bytes))))