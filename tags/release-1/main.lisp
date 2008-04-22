(in-package "ACL2")
(set-state-ok t)
(set-ignore-ok t)

(include-book "structures")
(include-book "operators")
(include-book "wav")
(include-book "filter")
(include-book "csv-writer")
(include-book "wav-gui")
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