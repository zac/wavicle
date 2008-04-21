(in-package "ACL2")
(include-book "world" :dir :teachpacks)

;----- Define the mouse world data structure -------------------------
(defstructure m-world 
  (left-channel   (:assert (true-listp left-channel)))
  (right-channel  (:assert (true-listp right-channel)))
)

;----- Define an initial world with some basic constants -------------
(defconst *height* 300)
(defconst *width*  800)

(defconst *seconds-per-tick* 1/10)

;----- Determine how many pixels to move forward between samples ----
;Works best when (length sample-list) is used as the parameter
(defun delta-x (num-samples)
  (/ *width* (- num-samples 1)))

;----- Determine the y-position of each sample ----------------------
; Y value is relative to the bottom of the window
; The 0-value for an 8-bit sample is the bottom of the window with a 256-value
;    sample at the top.
; The 0-value for a 16-bit sample is the middle of the window with room for
;    positive and negative values.
(defun calculate-y-value (sample-val)
      (- *height* (+ (/ *height* 2)
                     (* (/ *height* 2)
                        sample-val))))

;----- Split a list of audio samples into left and right channels ----
;(defun get-nth-channel (sample-list channel-number num-channels)
;  (if (nthcdr num-channels sample-list)
;      (cons (car (nthcdr channel-number sample-list))
;            (get-nth-channel (nthcdr (- num-channels 1) sample-list)
;                              channel-number
;                              num-channels)))
;      (car (nthcdr (- channel-number 1) sample-list)))

(defun get-left-channel (sample-list)
  (if (cdr sample-list)
      (cons (car sample-list) (get-left-channel (cddr sample-list)))
      (car sample-list)))

(defun get-right-channel (sample-list)
  (if (cddr sample-list)
      (cons (cadr sample-list) (get-right-channel (cddr sample-list)))
      (cons (cadr sample-list) nil)))

;----- Calculates the x and y coordinates for each sample -----------
;----- Outputs a list of cons pairs of x.y coordinates --------------
(defun calculate-graph (curr-x del-x sample-list)
  (if (cdr sample-list)
      (cons (cons curr-x (calculate-y-value (car sample-list)))
            (calculate-graph (+ curr-x del-x) del-x (cdr sample-list)))
      (cons (cons curr-x (calculate-y-value (car sample-list))) nil)))

;----- Generates a single graph for a single-channel wave ------------
(defun get-mono-graph (sample-list)
  (let ((del-x (delta-x (length sample-list))))
    (cons (calculate-graph 0 del-x sample-list)
          nil)))

;----- Generates a cons-pair of graphs -------------------------------
;----- ((left-channel-graph)(right-channel-graph)) -------------------
(defun get-stereo-graph (sample-list)
  (let ((del-x (delta-x (/ (length sample-list) 2))))
    (list (calculate-graph 0 del-x (get-left-channel sample-list))
          (calculate-graph 0 del-x (get-right-channel sample-list)))))

;----- Wrapper function that determines which of the above get-graph -
;----- functions to use, then returns the results --------------------
(defun get-graph (sample-list num-channels)
  (if (= num-channels 1)
      (get-mono-graph sample-list)
      (get-stereo-graph sample-list)))

;----- Extracts the required data from a wav-structure and calls get-graph ---
(defun calculate-wave (wav)
  (let* ((n (* (floor (/ (len ratlist) 1000) 2)))
         (sample-list (car (nthcdr 14 wav)))
         (small-list (shorten-list sample-list n 0))
         (num-channels (car (nthcdr 7 wav))))
    (get-graph small-list num-channels)))

;----- Functions to take the graph points and draw a line on the canvas----

(defun draw-line (list color scene)
  (if (null (cddr list))
      scene
      (let ((x1 (car (car list)))
            (y1 (cdr (car list)))
            (x2 (car (cadr list)))
            (y2 (cdr (cadr list))))
        (add-line (draw-line (cdr list) color scene) x1 y1 x2 y2 color))))

(defun draw-wave (w)
  (draw-line (m-world-left-channel w) 'blue
  (draw-line (m-world-right-channel w) 'red
  (empty-scene *width* *height*))))

(defun display-wave (wav)
  (let* ((solution (calculate-wave wav))
         (startx (big-bang *width* *height* *seconds-per-tick*
                           (m-world (car solution)
                                    (cadr solution))))
         (showfunction (on-redraw draw-wave)))
    t))