
'(%Z%%M% %R%.%L%.%B%.%S% %E% %Y%)
'(MIKKO-3 (3 / 6 - 1994) (6 : 51 : 12 91))
(defq *package* GKOE)

(defun gkoe ()
 (display-mode 5)
 (for
  (x 1 100)
  (color (remainder x 4))
  (point 0 0)
  (draw (* 3 x) 200))
 (for
  (i 1 20)
  (set_cursor i (* i 2))
  (_vtcolor i i)
  (print i))
 (readcc)
 (display-mode 6)
 (color 1)
 (for (x 1 100) (point 0 x) (draw 640 x))
 (color 0)
 (for (x 1 50) (point 0 x) (draw 600 x))
 (readcc)
 (for
  (x 1 200)
  (color x)
  (point (* x 3) 0)
  (draw (* x 3) 200))
 (readcc)
 (display-mode 3))

(defq GKOE (gkoe GKOE))
