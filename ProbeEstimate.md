# Design #

Here is the design of the project.

  * WAV Parser
    * parse-wav-file
    * bytes->integer
    * bytes->string
    * bytes->wav-packet
    * parse-audio-command-file
  * Operator Handler
    * get-filter
    * apply-operator-list
      * apply-delay
      * apply-cut
      * apply-boost
      * apply-overdub
      * apply-fade-in
      * apply-fade-out
      * apply-fuzz
      * apply-chipmunk
      * apply-filter
  * WAV Output
    * put-signal
    * display-signal
    * display-signal-gui

# PROBE #