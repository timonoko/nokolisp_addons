
'(%Z%%M% %R%.%L%.%B%.%S% %E% %Y%)
'(MIKKO-3 (19 / 2 - 1996) (18 : 48 : 7 98))
(defq *package* TIF)

(defun patch
 (f)
 (setq temp (open f))
 (LSEEK temp 0 '(0 . 42))
 (out temp)
 (printc (low-byte 480))
 (printc (high-byte 480))
 (close (out))
 (out 0))

(defun pienennys
 (f f2)
 (in (open f))
 (out (setq f2 (create f2)))
 (repeat-times 42 (printc (readc-bin)))
 (readc-bin)
 (readc-bin)
 (printc (low-byte 480))
 (printc (high-byte 480))
 (repeat-times 212 (printc (readc-bin)))
 (for
  (y 1 480)
  (out 0)
  (print y)
  (sp)
  (out f2)
  (for (x 0 511) (printc (readc-bin))))
 (close (in))
 (close (out))
 (in 0)
 (out 0))

(defun erot ()
 (for
  (x 0 127)
  (if
   (not (= (nth x eka.tif) (nth x iso.tif)))
   (progn (print x) (sp)))))

(defun head
 (f)
 (in (open f))
 (hex t)
 (for
  (x 0 127)
  (if
   (zerop (remainder x 16))
   (progn (cr) (print x) (print ':)))
  (sp)
  (push (print (readc-bin)) jono))
 (hex)
 (close (in))
 (in 0))

(defq TIF (patch pienennys erot head TIF))
