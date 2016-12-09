
'(%Z%%M% %R%.%L%.%B%.%S% %E% %Y%)
'(MIKKO-3 (2 / 6 - 1994) (23 : 14 : 3 28))
(defq *package* ASTEIKKO)

(defun text-point
 (x y)
 (set_cursor (1+ (/ y 8)) (1+ (/ x 8))))

(defun asteikko ()
 (setq DATA (read-from-file 'asteikko.dat))
 (setq where 1000)
 (mapc DATA
  (function
   (lambda
    (x)
    (when
     (> where 160)
     (unless (= where 1000) (readcc))
     (setq where 0)
     (display-mode 6))
    (cr)
    (text-point 0 where)
    (mapc
     (cdr x)
     (function (lambda (x) (print x) (sp))))
    (yks-asteikko 0 (car x) (+ 10 where))
    (setq where (+ 40 where)))))
 (readcc)
 (display-mode 3))

(defun yks-asteikko
 (start scale where)
 (flet
  ((x-point
    (x)
    (if
     (< start x (+ start scale))
     (cond
      ((< scale 50) (/ (* 640 (- x start)) scale))
      ((< scale 100)
       (/ (* 320 (- x start)) (/ scale 2)))
      (t (/ (* 80 (- x start)) (/ scale 8))))
     0)))
  (point 0 where)
  (draw 640 where)
  (for
   (i start (+ start scale))
   (point (x-point i) (+ 0 where))
   (draw (x-point i) (+ where 5))
   (cond
    ((zerop (remainder i 10))
     (draw
      (x-point i)
      (+ where (if (zerop (remainder i 50)) 15 12)))
     (when
      (and
       (< 0 (x-point i) 620)
       (or (< scale 300) (zerop (remainder i 50))))
      (text-point (x-point i) (+ where 16))
      (print (/ i 10))))
    ((zerop (remainder i 5))
     (draw (x-point i) (+ where 8)))))))

(defq ASTEIKKO (text-point asteikko yks-asteikko ASTEIKKO))
