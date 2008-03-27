(in-package "ACL2")
(include-book "io-utilities" :dir :teachpacks)
(set-state-ok t)
(include-book "world" :dir :teachpacks)
(include-book "list-utilities" :dir :teachpacks)

#|----------------------reading file -> list of strings---------------|#
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
              (mv (parse-list (tokens (list #\( #\) #\Newline) (str->chrs line))) state))))
#|-------------------END file->list of strings------------------------|#

(defstructure acf
  (output       (:assert (string-listp output)))
  (files        (:assert (string-listp files)))
  (commands     (:assert (string-listp commands))))

(defun keywords-output (word)
  (let ((keylist '("put-signal" "display-signal")))
        (if (member-equal word keylist)
            t
            nil)))

(defun get-vars (vars str)
  (if (consp vars)
      (cons (cons (car vars) (car str)) (get-vars (cdr vars) (cdr str)))
      nil))

(defun get-cmds (str vars)
  (if (consp str)
      (if (not (equal (car str) vars))
          (cons (car str) (get-cmds (cdr str) vars))
          nil)
      nil))
  
(defun line-parser(str)
  (let* ((output (if (keywords-output (caar str))
                     (car str)
                     nil))
         (file-vars (if (not (eq output nil))
                    (caddr str)
                    (cadr str)))
         (vars-list (get-vars (reverse file-vars) (reverse str)))
         (commands (get-cmds (nthcdr (len file-vars) (reverse str)) file-vars)))
    (acf output vars-list commands)))

(defun line->words(line)
  (if (consp line)
      (remove nil (cons (words (car line)) (line->words (cdr line))))
      nil))

(defun parser (file state)
  (mv-let (str state)
          (readfile file state)
          (mv (line-parser (line->words str)) state)))