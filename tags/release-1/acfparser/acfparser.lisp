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

(defun replace-variable (word vars-list file-vars)
  (if (consp vars-list)
      (if (member-equal word file-vars)
          (if (equal (caar vars-list) word)
              (caddar vars-list)
              (replace-variable word (cdr vars-list) file-vars))
          word)
      nil))
  
(defun replace-vars-helper (cmd vars-list file-vars)
  (if (consp cmd)
      (cons (replace-variable (car cmd) vars-list file-vars) (replace-vars-helper (cdr cmd) vars-list file-vars))
      nil))

(defun replace-vars (cmds vars-list file-vars)
  (if (consp cmds)
      (cons (replace-vars-helper (car cmds) vars-list file-vars) (replace-vars (cdr cmds) vars-list file-vars))
      nil))

(defun line-parser(str)
  (let* ((output (if (keywords-output (caar str))
                     (car str)
                     nil))
         (file-vars (if (not (eq output nil))
                    (caddr str)
                    (cadr str)))
         (vars-list (get-vars (reverse file-vars) (reverse str)))
         (commands (get-cmds (nthcdr (len file-vars) (reverse str)) file-vars))
         (newcmds (replace-vars commands vars-list file-vars)))
    (acf output newcmds)))

(defun line->words(line)
  (if (consp line)
      (remove nil (cons (words (car line)) (line->words (cdr line))))
      nil))

(defun parser (file state)
  (mv-let (str state)
          (readfile file state)
          (mv (line-parser (line->words str)) state)))
