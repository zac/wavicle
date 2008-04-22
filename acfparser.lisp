(in-package "ACL2")
;(include-book "io-utilities" :dir :teachpacks)

#|------------------------Reading the ACF file--------------------------------|#
(defun parse-lines (line)
  (let* ((info (tokens (list #\tab) line))
         (cmds (chrs->str (car info))))
    cmds))

(defun parse-list (l)
  (if (consp l)
      (cons (parse-lines (car l)) (parse-list (cdr l)))
      nil))

(defun readfile (file state)
  (mv-let (line error state)
          (file->string file state)
          (if error
              (mv error state)
              (mv (parse-list (tokens (list #\( #\) #\Newline) 
                                      (str->chrs line))) state))))
#|--------------------------------End of this code----------------------------|#

#|------------------------Required Structures and Keywords--------------------|#
;Function that checks for output types.
(defun keywords-output (word)
  (let ((keylist '("put-signal" "display-signal")))
        (if (member-equal word keylist)
            t
            nil)))
;Function that checks effects words.
(defun cmd-keys (word)
  (let ((keylist '("boost" "fuzz" "delay" "overdub" "echo" "fade-in" "fade-out" 
                           "cut" "chipmunk" "reverse" "filter")))
        (if (member-equal word keylist)
            t
            nil)))
#|--------------------------------End of this code----------------------------|#


#|------------------------------------Get-Vars--------------------------------|#
;Function that gets the list of file variables from the
;parameters of the lamda function.
(defun get-vars (vars str)
  (if (consp vars)
      (cons (cons (car vars) (car str)) (get-vars (cdr vars) (cdr str)))
      nil))
#|--------------------------------End of this code----------------------------|#

#|---------------------Replaces file variables with actual file names---------|#
;Helper function for replace-vars-helper
(defun replace-variable (word vars-list file-vars)
  (if (consp vars-list)
      (if (member-equal word file-vars)
          (if (equal (caar vars-list) word)
              (caddar vars-list)
              (replace-variable word (cdr vars-list) file-vars))
          word)
      nil))

;Helper function for replace-vars 
(defun replace-vars-helper (cmd vars-list file-vars)
  (if (consp cmd)
      (cons (replace-variable (car cmd) vars-list file-vars) 
            (replace-vars-helper (cdr cmd) vars-list file-vars))
      nil))

;Function that initially receives the list of commands,
;list of variables, and list of file names corresponding
;with file names
(defun replace-vars (cmds vars-list file-vars)
  (if (consp cmds)
      (cons (replace-vars-helper (car cmds) vars-list file-vars) 
            (replace-vars (cdr cmds) vars-list file-vars))
      nil))
#|--------------------------------End of this code----------------------------|#

#|-------------------------Get last element in a list-------------------------|#
; (get-last)
; Function that gets the last element in the list.
;
; xs - The list from which you want to retrieve the last element
(defun get-last (xs)
  (if (endp xs)
      nil
      (car (nthcdr (- (length xs) 1) xs))))
#|--------------------------------End of this code----------------------------|#

#|---------------------------------Verify Commands----------------------------|#
;Checks if the commands in the list of lamda functions have overdub.
;If it does contain overdub, makes sure the commands are properly arranged.
(defun get-cmds (str)
  (if (consp str)
      (if (string-equal "overdub" (caar str))
          (if (equal (len (car str)) 1)
              (cons (cons (caar str) (get-last str)) 
                    (get-cmds (butlast (cdr str) 1)))
              (cons (car str) (get-cmds (cdr str))))
          (cons (car str) (get-cmds (cdr str))))
      nil))
#|--------------------------------End of this code----------------------------|#

#|--------------------------------Create ACF Structure------------------------|#
;Returns an acf structure that contains the output type
;and the list of commands
(defun line-parser(str)
  (if (equal str nil)
      "No data available to parse"
      (let* ((output (if (keywords-output (caar str))
                     (car str)
                     nil))
         (file-vars (if (not (eq output nil))
                    (caddr str)
                    (cadr str)))
         (vars-list (get-vars (reverse file-vars) (reverse str)))
         (cmds-hlpr (nthcdr (len file-vars) (reverse (nthcdr 3 str))))
         (commands (reverse (get-cmds (reverse cmds-hlpr))))
         (newcmds (if (equal (len commands) 0)
                      (if (string-equal "display-signal" (car output))
                          (cdar vars-list)
                          nil)
                     (replace-vars commands vars-list file-vars))))
   (acf output newcmds))))
#|--------------------------------End of this code----------------------------|#

#|--------------------------Driver to parse acf file--------------------------|#
;Function that takes out empty spaces in the original string
;that is received after reading the file.
(defun line->words(line)
  (if (consp line)
      (remove nil (cons (words (car line)) (line->words (cdr line))))
      nil))

;Main function that returns the acf structure containing
;the required information to run the program.
(defun parser (file state)
  (mv-let (str state)
          (readfile file state)
          (mv (line-parser (line->words str)) state)))
#|--------------------------------End of this code----------------------------|#

#|-------------------------------Parser/Operator Integration------------------|#

; (acf-function-h)
; Helper function for acf-function that translates what is returned from
; the parser into function calls that will alter a wav file
;
; commands - The list of commands to be performed on the wav file
; wav - The wav struture representing the wav file to be modified
; state - The state variable
(defun acf-function-h (commands wav state)
  (if (endp commands)
      (mv wav state)
      (let* ((args (car commands))
            (arg1 (car args))
            (arg2 (cadr args))
            (arg3 (caddr args)))
        (cond ((string-equal arg1 "boost")
               (mv (boost (str->rat arg2) (mv-let (wav2 state)
                                   (acf-function-h (cdr commands) wav state)
                                   wav2)) state))
              ((string-equal arg1 "fuzz")
               (mv (fuzz (str->rat arg2) (mv-let (wav2 state)
                                   (acf-function-h (cdr commands) wav state)
                                   wav2)) state))
              ((string-equal arg1 "delay")
               (mv (delay (str->rat arg2) (mv-let (wav2 state)
                                   (acf-function-h (cdr commands) wav state)
                                   wav2)) state))
              ((string-equal arg1 "overdub")
               (if (not (cmd-keys arg2))
                   (mv (overdub (mv-let (wav3 state)
                                 (acf-function-h (cdr commands) wav state) wav3)
                                (mv-let (wav2 state)
                                 (read-wav arg2 state) wav2)) state)
                   (mv (overdub (mv-let (wav6 state) 
                                 (acf-function-h (cdr commands) wav state) wav6)
                                 (mv-let (wav4 state)
                                  (acf-function-h (list (butlast (cdr args) 1))
                                (mv-let (wav5 state)
                                 (read-wav (get-last args) state) wav5)
                                 state) wav4)) state)))
              ((string-equal arg1 "echo")
               (mv (echo (str->rat arg2) (str->rat arg3) (mv-let (wav2 state)
                                   (acf-function-h (cdr commands) wav state)
                                   wav2)) state))
              ((string-equal arg1 "fade-in")
               (mv (fade-in (str->rat arg2) (mv-let (wav2 state)
                                   (acf-function-h (cdr commands) wav state)
                                   wav2)) state))
              ((string-equal arg1 "fade-out")
               (mv (fade-out (str->rat arg2) (mv-let (wav2 state)
                                   (acf-function-h (cdr commands) wav state)
                                   wav2)) state))
              ((string-equal arg1 "cut")
               (mv (cut (str->rat arg2) (str->rat arg3) (mv-let (wav2 state)
                                   (acf-function-h (cdr commands) wav state)
                                   wav2)) state))
              ((string-equal arg1 "chipmunk")
               (mv (chipmunk (str->rat arg2) (mv-let (wav2 state)
                                   (acf-function-h (cdr commands) wav state)
                                   wav2)) state))
              ((string-equal arg1 "reverse")
               (mv (audio-reverse (mv-let (wav2 state)
                                   (acf-function-h (cdr commands) wav state)
                                   wav2)) state))
              ((string-equal arg1 "filter")
               (mv (filter (car (read-filter arg2 state))
                           (mv-let (wav2 state)
                                   (acf-function-h (cdr commands) wav state)
                                   wav2)) state))))))

; (acf-function)
; Sets up the necessary conditions for acf-function-h to correctly call
; functions to alter a wav file.
;
; acf - The parsed ACF from (parser acf state)
; state - the state variable
(defun acf-function (acf state)
  (let* ((output (acf-output acf))
         (action (car output))
         (file (cadr output))
         (commands (acf-commands acf)))
    (if (not (stringp (get-last (car commands))))
        (mv "Bad input filename" state)
    (mv-let (wav state)
            (acf-function-h (reverse (cons (butlast (car commands) 1)
                                           (cdr commands)))
                            (mv-let (wav state)
                                    (read-wav (get-last (car commands)) state)
                                    wav) state)
            (cond ((string-equal action "put-signal")
                   (if (not (stringp file))
                       (mv "Bad output filename" state)
                       (write-wav wav file state)))
                  ((string-equal action "display-signal")
                   (if (not (stringp file))
                       (mv "Bad csv filename" state)
                   (let ((temp (display-wave wav)))
                         (write-csv file (wav-file-data wav) state)))))))))
#|-----------------------------End of this Code-------------------------------|#