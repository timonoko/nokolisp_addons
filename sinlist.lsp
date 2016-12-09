
'(%Z%%M% %R%.%L%.%B%.%S% %E% %Y%)
'(MIKKO-3 (19 / 6 - 1994) (15 : 4 : 31 22))
(defq *package* SINLIST)

(defun sinlist ()
 (out (create 'sinlist.asm))
 (for
  (x 0 350 10)
  (cr)
  (tab 8)
  (print 'db)
  (sp)
  (print (/ (+ 100 (sin x)) 2))
  (tab 16)
  (print ';)
  (sp)
  (print '=sin)
  (print (list x)))
 (close (out))
 (out 0))

(defun sin
 (x)
 (cond
  ((< -1 x 91)
   (if
    (zerop (remainder x 10))
    (cadr
     (assoc x
      (quote
       ((0 0)
        (10 17)
        (20 34)
        (30 50)
        (40 64)
        (50 76)
        (60 86)
        (70 93)
        (80 98)
        (90 100)))))
    (let
     ((i (* (/ x 10) 10))
      (r (remainder x 10)))
     (+
      (sin i)
      (/
       (* r (- (sin (+ 10 i)) (sin i)))
       10)))))
  ((< 90 x 181) (sin (- 180 x)))
  ((< 180 x 361) (minus (sin (- x 180))))
  ((< x 0) (minus (sin (minus x))))
  (t (sin (- x 360)))))

(defq SINLIST (sinlist sin SINLIST))
