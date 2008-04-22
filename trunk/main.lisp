(in-package "ACL2")
(include-book "structures")
(include-book "operators")
(include-book "wav")
(include-book "wav-gui")
(include-book "filter")
(include-book "csv-writer")
(include-book "acfparser")

; (main file state)
; Runs wavicle using the acf specified.
;
; file - The path to the acf
; state - The state variable
(defun main (file state)
  (mv-let (acf state)
          (parser file state)
          (acf-function acf state)))