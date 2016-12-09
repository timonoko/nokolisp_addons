
'(%Z%%M% %R%.%L%.%B%.%S% %E% %Y%)
'(MIKKO-3 (19 / 6 - 1994) (21 : 0 : 9 77))
(defq *package* REAL)

(defun real*int
 (r i)
 (real r)
 (cond
  ((zerop i) '(0 . 0))
  ((zerop (logand i 1))
   (real*int (real-plus r r) (/ i 2)))
  (t (real-plus r (real*int r (1- i))))))

(defun print-real
 (x)
 (real x)
 (print (car x))
 (let
  ((y (abs (cdr x))) (i 1000))
  (printc 46)
  (while
   (> y 0)
   (print (/ y i))
   (setq y (remainder y i))
   (setq i (/ i 10)))))

(defun real-quotient
 (x y)
 (real x)
 (real y)
 (cond
  ((zerop (cdr y))
   (if
    (zerop (car y))
    (progn
     (print (list 'DIVIDE 'BY 'ZERO x))
     (cons 32767 9999))
    (cons
     (/ (car x) (car y))
     (+
      (/&*10000 (remainder (car x) (car y)) (car y))
      (/ (cdr x) (car y))))))
  ((zerop (car y)) (real-quotient (real-times 10000 x) (cdr y)))))

(defun real-times
 (x y)
 (real x)
 (real y)
 (cond ((zerop (cdr x)) (real*int x (car y)))))

(defmacro real
 (x)
 (` if
  (numberp , x)
  (setq , x (cons , x 0))))

(defun real-plus
 (x y)
 (%tracep% real-plus
  (x y)
  ((real x)
   (real y)
   (cons
    (+
     (car x)
     (car y)
     (/ (+ (cdr x) (cdr y)) 10000))
    (remainder (+ (cdr x) (cdr y)) 10000)))))

(defq REAL (real*int print-real real-quotient real-times real real-plus REAL))
