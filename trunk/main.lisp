(in-package "ACL2")
(include-book "structures")
(include-book "operators")
(include-book "wav")
(include-book "wav-gui")
(include-book "filter")
(include-book "acfparser")

(defun main (file state)
  (mv-let (acf state)
          (parser file state)
          (acf-function acf state)))