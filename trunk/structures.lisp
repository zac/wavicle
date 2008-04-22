(in-package "ACL2")

;(include-book "world" :dir :teachpacks)

; wav-file structure
;    chunk-id - This is a string containing the ASCII letters "RIFF" to identify
;      the file as an audio file.
;    chunk-size - This is an integer to dosp;lay the size of the entire file, in
;      bytes, following the chunk-size value.
;    format - A string containing the ASCII letters "WAVE" to identify the file
;      as a waveform audio file.
;    subchunk-1-id - A string containing the ASCII letters "fmt ".
;    subchunk-1-size - An integer value for the size of the subchunk-1 part of
;      the file header following this value. This value is always 16 for
;      standard PCM audio file.
;    audio-format - This is a number value representing the compression-type for
;      the wav-file's data. Standard PCM files always have a value of 1 to
;      indicate that there is no compression.
;    num-channels - This is an integer value for the number of audio channels
;      in the .wav file. Mono = 1, stereo = 2, etc.
;    sample-rate - This is an integer value to describe the number of audio data
;      samples that the file stores per second of audio playback.
;    byte-rate - An integer value to describe the number of bytes of audio data
;      that the file stores per second of playback.
;      byte-rate = sample-rate * num-channels * bits-per-sample/8
;    block-align - An integer value for the number of bytes per block of data
;      for a single sample on all channels.
;      block-align = num-channels * bits-per-sample/8
;    bits-per-sample - An integer value for the number of bits per sample in a
;      single audio sample. This is used to determine the byte-rate and block-
;      align values.
;    subchunk-2-id - A string containing the ASCII characters "data". This
;      identifies the beginning of the data section of the wav-file structure.
;    subchunk-2-size - An integer value for the size, in bytes, of the data
;      section of the wav-file.
;      subchunk-2-size = (length data) * num-channels * bits-per-sample/8
;    data - This is list of audio samples read from the .wav file. wAviCLe
;      stores these values as a list of floating point numbers between -1
;      (minimum possible sample value) and 1 (maximum possible sample value).

(defstructure wav-file
  (chunk-id (:assert (stringp chunk-id)))
  (chunk-size (:assert (integerp chunk-size)))
  (format (:assert (stringp format)))
  (subchunk-1-id (:assert (listp subchunk-1-id)))
  ;Make sure it is PCM.
  (subchunk-1-size (:assert (integerp subchunk-1-size)))
  (audio-format (:assert (= audio-format 1)))
  ;Mono = 1, Stereo = 2, etc.
  (num-channels (:assert (integerp num-channels)))
  ;8000, 44100, etc.
  (sample-rate (:assert (integerp sample-rate)))
  ;== sample-rate * num-channels * (bits-per-sample / 8)
  (byte-rate (:assert (integerp byte-rate)))
  ;== num-channels * (bits-per-sample / 8)
  (block-align (:assert (integerp block-align)))
  ;8 bits = 8, 16 bits = 16, etc.
  (bits-per-sample (:assert (integerp bits-per-sample)))
  (subchunk-2-id (:assert (listp subchunk-2-id)))
  (subchunk-2-size (:assert (integerp subchunk-2-size)))
  (data (:assert (listp data))))

;Structure that contains the required information to run the program.
; acf structure
;    output - A list of strings representing commands that the acf parser will
;      read and interpret into a list of commands to execute.
;    commands - A list of commands that the acf parser has read from a file for
;      the main program to execute.
(defstructure acf
  (output       (:assert (string-listp output)))
  (commands     (:assert (true-listp commands))))