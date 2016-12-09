
'(%Z%%M% %R%.%L%.%B%.%S% %E% %Y%)
'(MIKKO-3 (12 / 6 - 1994) (16 : 55 : 0 6))
(defq *package* SEEMEM)

(defun see-mem
 (s)
 (for
  (x 0 32000)
  (setq c (peek s x))
  (when
   (< 31 c 127)
   (when
    (zerop (remainder x 32))
    (hex t)
    (cr)
    (print s)
    (print ':)
    (print x)
    (sp)
    (hex nil))
   (printc c))))

(defq SEEMEM (see-mem SEEMEM))
