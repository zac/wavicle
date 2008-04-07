(in-package "ACL2")
(include-book "doublecheck" :dir :teachpacks)

(include-book "wav")

(defproperty 2scomp-test 20
  ((bits (random-int-between 4 32)
         t)
   (num (random-int-between 1 (expt 2 bits))
        t))
  (equal num (2scomp->unsigned (unsigned->2scomp num bits) bits)))

(defproperty intbytes-test 20
  ((bytes (random-int-between 2 16)
         t)
   (num (random-int-between 0 (expt 2 (- (* 8 bytes) 1)))
        t))
  (equal num (bytes->integer (integer->bytes num bytes))))

(check-properties)

