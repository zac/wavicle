(in-package "ACL2")
(include-book "world" :dir :teachpacks)

;----- Define the mouse world data structure -------------------------
; (m-world left-channel right-channel)
;    left-channel - A list of graph points for displaying the left channel of a
;      wav-structure's audio data.
;    right-channel - A list of graph points for displaying the right channel of
;      a wav-structure's audio data.
; This structure is made to store the points for the graph of a waveform
; separated into left and right channels.
(defstructure m-world 
  (left-channel   (:assert (true-listp left-channel)))
  (right-channel  (:assert (true-listp right-channel)))
)

;----- Define an initial world with some basic constants -------------
; *height* - A pre-defined height for the display window
(defconst *height* 300)
; *width* - A pre-defined width for the display window.
(defconst *width*  800)

; *second-per-tick* - A pre-defined time unit for the big-bang function
; (Not really used but important for running the GUI)
(defconst *seconds-per-tick* 1/10)

;----- Determine how many pixels to move forward between samples ----
; (delta-x num-samples)
;    num-samples - An integer value showing the number of samples to be
;      displayed on the graph.
; This function determines how far to move the value of x for the display graphs
; in order to evenly space all of the samples horizontally on the graph.
(defun delta-x (num-samples)
  (/ *width* (- num-samples 1)))

;----- Determine the y-position of each sample ----------------------
; (calculate-y-value sample-val)
;    sample-val - A floating point value between 0 and 1 that represents an
;      audio sample taken from a wav-structure.
; This function determines the y value of an audio sample in order to figure out
; where to draw it on the graph.
(defun calculate-y-value (sample-val)
      (- *height* (+ (/ *height* 2)
                     (* (/ *height* 2)
                        sample-val))))

;----- Separate a channel list into left and right channels ----------
; (get-left-channel sample-list)
;    sample-list - A list of floating-point values that represent an audio
;      waveform taken from a wav-structure.
; This function seperates the all of the odd-indexed values from a list of audio
; samples and puts them into a list that represents the left channel of a .wav
; file.
(defun get-left-channel (sample-list)
  (if (cdr sample-list)
      (cons (car sample-list) (get-left-channel (cddr sample-list)))
      (car sample-list)))

; (get-right-channel sample-list)
;    sample-list - A list of floating-point values that represent an audio
;      waveform taken from a wav-structure.
; This function seperates the all of the even-indexed values from a list of
; audio samples and puts them into a list that represents the right channel of
; a .wav file.
(defun get-right-channel (sample-list)
  (if (cddr sample-list)
      (cons (cadr sample-list) (get-right-channel (cddr sample-list)))
      (cons (cadr sample-list) nil)))

;----- Calculates the x and y coordinates for each sample -----------
;----- Outputs a list of cons pairs of x.y coordinates --------------
; (calculate-graph (curr-x del-x sample-list)
;    curr-x - An floating point aggregate counter for determining the x-value
;      of the current point on the graph.
;    del-x - A floating point increment value to add to curr-x to determine the
;      x-value of the next point on the graph.
;    sample-list - A list of floating point value audio-samples extracted from
;      a wav-structure
; This function generates a list of graph points for a single audio channel
; extracted from a wav-structure on a graph of size *height* x *width*.
(defun calculate-graph (curr-x del-x sample-list)
  (if (cdr sample-list)
      (cons (cons curr-x (calculate-y-value (car sample-list)))
            (calculate-graph (+ curr-x del-x) del-x (cdr sample-list)))
      (cons (cons curr-x (calculate-y-value (car sample-list))) nil)))

;----- Generates a single graph for a single-channel wave ------------
; (get-mono-graph sample-list)
;    sample-list - A list of samples extracted from a wav-structure
; This function runs the calculate-graph function to generate a list of graph
; points for the left-channel of the m-world structure. The right channel is
; written as an empty list.
(defun get-mono-graph (sample-list)
  (let ((del-x (delta-x (length sample-list))))
    (cons (calculate-graph 0 del-x sample-list)
          nil)))

;----- Generates a cons-pair of graphs -------------------------------
; (get-stereo-graph sample-list)
;    sample-list - A list of samples extracted from a wav-structure
; This function splits sample-list into left and right channels, then runs
; calculate-graph on both channels to generate a cons-pair of graph point lists.
; The first and second elements of the cons-pair represent the left and right
; channels respectively.
(defun get-stereo-graph (sample-list)
  (let ((del-x (delta-x (/ (length sample-list) 2))))
    (list (calculate-graph 0 del-x (get-left-channel sample-list))
          (calculate-graph 0 del-x (get-right-channel sample-list)))))

;----- Wrapper function that determines which of the above get-graph -
;----- functions to use, then returns the results --------------------
; (get-graph sample-list num-channels)
;    sample-list - A list of samples extracted from a wav-structure
;    num-channels - An integer value extracted from a wav-structure that
;      describes the number of audio channels stored in sample-list.
;      WARNING: This function only works for num-channel values of 1 or 2!
; This function uses the number of channels in a wav-structure to determine
; which graph calculating function to use, then passes sample-list as a
; parameter to that graphing function.
(defun get-graph (sample-list num-channels)
  (if (= num-channels 1)
      (get-mono-graph sample-list)
      (get-stereo-graph sample-list)))

;----- Extracts the required data from a wav-structure and calls get-graph ---
; (calculate-wave wav)
;    wav - A wav-structure representing data taken from a .wav file.
; This function extracts the needed data from a wav-structure. It then shortens
; the sample-list to provide a more managable, representative list of samples
; and sends it to the get-graph function to create a pair of graphs based on
; sample-list.
(defun calculate-wave (wav)
  (let* ((sample-list (car (nthcdr 14 wav)))
         (n (* (floor (len sample-list) 2000) 2))
         (small-list (shorten-list sample-list n 0))
         (num-channels (car (nthcdr 7 wav))))
    (get-graph small-list num-channels)))

;----- Functions to take the graph points and draw a line on the canvas----
; (draw-line list color scene)
;    list - This is a list of graph points with each point consisting of
;      cons-pair of x and y coordinates.
;    color - A string describing which color the line is to be drawn in.
;    scene - An existing scene to draw the line in.
; This function draws a line representing an audio channel in on an existing
; scene passed to it as a parameter. The line is added to the scene and the
; completed scene is returned by the function.
(defun draw-line (list color scene)
  (if (null (cddr list))
      scene
      (let ((x1 (car (car list)))
            (y1 (cdr (car list)))
            (x2 (car (cadr list)))
            (y2 (cdr (cadr list))))
        (add-line (draw-line (cdr list) color scene) x1 y1 x2 y2 color))))

; (draw-wave)
; This function is passed as a parameter for the on-redraw function. The
; function is creates an empty-scene of size *height* x *width*. It then uses
; the draw-line functions to add a line for the right channel of the graph in
; red and the left channel of the graph in blue, then draws the resulting scene
; to the display window.
(defun draw-wave (w)
  (draw-line (m-world-left-channel w) 'blue
  (draw-line (m-world-right-channel w) 'red
  (empty-scene *width* *height*))))

; (display-wave wav)
;    wav - A wav-structure representing data taken from a .wav file.
; This function calculates sets of graph points from a wav-structure. It adds
; these sets of graph points to an m-world structure and uses the on-redraw
; function to display the graphs to the screen.
(defun display-wave (wav)
  (let* ((solution (calculate-wave wav))
         (startx (big-bang *width* *height* *seconds-per-tick*
                           (m-world (car solution)
                                    (cadr solution))))
         (showfunction (on-redraw draw-wave)))
    t))