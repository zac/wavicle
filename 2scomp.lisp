; (unsigned->2scomp num numbits)
;    num - An unsigned integer to be converted.
;    numbits - The desired number of bits. Used to determine the sign bit.
;  This function determines the 2's Compliment value of an unsigned series of
;  bits that is numbits in length.
(defun unsigned->2scomp (num numbits)
  (let ((pivot (expt 2 (- numbits 1))))
    (if (< num pivot)
        num
        (- 0 (- pivot (- num pivot))))))

; (2scomp->unsigned num numbits)
;    num - An unsigned integer to be converted.
;    numbits - The desired number of bits. Used to determine the sign bit.
;  This function determines the unsigned value of an 2's Compliment series of
;  bits that is numbits in length.
(defun 2scomp->unsigned (num numbits)
  (let ((pivot (expt 2 (- numbits 1))))
    (if (posp num)
        num
        (+ pivot (- pivot (- 0 num))))))