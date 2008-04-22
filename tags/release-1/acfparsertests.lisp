(in-package "ACL2")
(include-book "main")

#|----------------Helper functions required to run the tests------------------|#
;sample acf files to read.
(defun sample1 ()
  "(put-signal output.wav
                ((lambda (f input1 input2)
                       (overdub (filter f input1)
                                (chipmunk 2 input2)))
             (get-filter test.flt)
             (get-signal samples/voice1.wav)
             (get-signal samples/voice2.wav)))")

;sample acf file to test nested overdub arguments
(defun sample2 ()
  "(put-signal output.wav
                ((lambda (f input1 input2)
                       (overdub (filter f input1)
                                (chipmunk 2 (fade-out 2 input2))))
             (get-filter test.flt)
             (get-signal samples/voice1.wav)
             (get-signal samples/voice2.wav)))")

;sample acf file to test filepath in quotes
(defun sample3 ()
  "(put-signal \"output.wav\"
                ((lambda (f input1 input2)
                       (overdub (filter f input1)
                                (chipmunk 2 input2)))
             (get-filter test.flt)
             (get-signal samples/voice 1.wav)
             (get-signal samples/voice 2.wav)))")

;sample acf file to test empty lamdba function
(defun sample4 ()
  "(display-signal output.wav
                ((lambda (input1))
             (get-signal samples/voice1.wav))))")
;function that will take a string and run the parser on it
;
;str - acf file in a string form.
(defun runparser (str)
  (let ((sample (parse-list (tokens (list #\( #\) #\Newline) 
                                      (str->chrs str)))))
    (line-parser (line->words sample))))
#|----------------------------------------------------------------------------|#

#|-----------------------ACF Parser Unit Tests--------------------------------|#
;function that returns t or nil depending on the
;similarities of the expected and actual results.
;from running the sample1.

(defun test-sample1 ()
  (let ((expected (acf '("put-signal" "output.wav") 
                     '(("filter" "test.flt" "samples/voice1.wav") 
                       ("overdub" "chipmunk" "2" "samples/voice2.wav"))))
        (actual (runparser (sample1 ))))
    (if (equal expected actual)
        t
        nil)))

;function that returns t or nil depending on the
;similarities of the expected and actual results.
;from running the sample2.
(defun test-sample2 ()
  (let ((expected (acf '("put-signal" "output.wav") 
                     '(("fade-out" "2" "samples/voice2.wav") 
                       ("chipmunk" "2") 
                       ("overdub" "filter" "test.flt" "samples/voice1.wav"))))
        (actual (runparser (sample2 ))))
    (if (equal expected actual)
        t
        nil)))

;function that returns t or nil depending on the
;similarities of the expected and actual results.
;from running the sample3.
(defun test-sample3 ()
  (let ((expected (acf '("put-signal" "output.wav") 
                     '(("filter" "test.flt" "samples/voice 1.wav") 
                       ("overdub" "chipmunk" "2" "samples/voice 2.wav"))))
        (actual (runparser (sample3 ))))
    (if (equal expected actual)
        t
        nil)))

;function that returns t or nil depending on the
;similarities of the expected and actual results.
;from running the sample4.
(defun test-sample4 ()
  (let ((expected (acf '("display-signal" "output.wav") 
                     '("get-signal" "samples/voice1.wav")))
        (actual (runparser (sample4 ))))
    (if (equal expected actual)
        t
        nil)))
#|----------------------------------------------------------------------------|#