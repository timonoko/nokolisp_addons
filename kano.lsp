
'(%Z%%M% %R%.%L%.%B%.%S% %E% %Y%)
'(MIKKO-3 (23 / 6 - 1994) (19 : 4 : 1 54))
(defq *package* kano)

(defun fib
 (x)
 (if
  (< x 2)
  x
  (+ (fib (1- x)) (fib (- x 2)))))

(defun stereo ()
 (setq WIDTH 60)
 (setq vino 5)
 (repeat
  (display-mode 6)
  (setq LENTGH 500)
  (setq vasen 200)
  (setq oikea 400)
  (for
   (z 0 500 20)
   (kaari z
    (+ vasen (/ z (1+ vino)))
    (- 200 (/ z 5))
    3))
  (for
   (z 0 500 20)
   (kaari z
    (+ oikea (/ z vino))
    (- 200 (/ z 5))
    3))
  (point vasen 200)
  (draw
   (+ vasen (/ 500 (1+ vino)))
   (- 200 (/ 500 5)))
  (point oikea 200)
  (draw
   (+ oikea (/ 500 vino))
   (- 200 (/ 500 5)))
  (case
   (readcc)
   (43 (setq WIDTH (+ WIDTH 5)) nil)
   (45 (setq WIDTH (- WIDTH 5)) nil)
   (118 (setq vino (1+ vino)) nil)
   (t t))))

(defun any-rima
 (z wid len)
 (setq wid (/ wid 2))
 (setq len (/ len 20))
 (setq z (/ (- z (/ LENTGH 2)) 10))
 (- wid
  (/ (* z z) (/ (* len len) wid))))

(defun solve-x^2
 (wid len)
 (setq len (/ len 10))
 (/ (* len len) wid))

(defun iso-kuva
 (scale)
 (unless scale (setq scale 2))
 (display-mode 6)
 (for
  (z 500 0 -5)
  (kaari z
   (+ 30 (/ z 1))
   (- 200 (/ z 10))
   scale t)))

(defun fast ()
 (if (< (reclaim) 5000) (more-memory))
 (mapc kano compile-all))

(defun kansi2-y
 (z)
 (cond
  ((< z 251) (+ 20 (/ z 40)))
  ((< 351 z) (+ 20 (/ (- 500 z) 40)))
  (t 20)))

(defun kansi2-x
 (z)
 (if (< 250 z 350) (kansi1-x z) 0))

(defun kansi1-y () 20)

(defun kansi1-x
 (x)
 (setq x (/ (- x 250) 10))
 (- 23 (/ (* x x) 25)))

(defun pohja-y
 (x)
 (if
  (< 3 x 497)
  (progn
   (setq x (/ (- x 250) 10))
   (+ (/ (* x x) 50) 1))
  20))

(defun pohja-x
 (x)
 (setq x (/ (- x 250) 10))
 (- 23 (/ (* x x) 25)))

(defun kaari
 (z x y scale half)
 (let
  ((points
    (quote
     ((koli-x koli-y)
      (pohja-x pohja-y)
      (kulma-x kulma-y)
      (reuna-x reuna-y)
      (kansi1-x kansi1-y)
      (kansi2-x kansi2-y)))))
  (point x (- y (* scale (koli-y z))))
  (mapc points
   (function
    (lambda
     (p)
     (draw
      (+ x (* scale ((eval (car p)) z)))
      (- y (* scale ((eval (cadr p)) z)))))))
  (point x (- y (* scale (koli-y z))))
  (unless half
   (mapc points
    (function
     (lambda
      (p)
      (draw
       (- x (* scale ((eval (car p)) z)))
       (- y (* scale ((eval (cadr p)) z))))))))))

(defun koli-y
 (x)
 (cond
  ((< x 5) (- 20 (* 4 x)))
  ((> x 495) (- 20 (* 4 (- 500 x))))
  (t 0)))

(defun koli-x
 (x)
 (if
  (< 5 x 495)
  (progn
   (setq x (/ (- x 250) 10))
   (- 9 (/ (* x x) 60)))
  0))

(defun kulma-y
 (x)
 (setq x (/ (- x 250) 10))
 (+ (/ (* x x) 50) 5))

(defun rima
 (ope x y f not-eka)
 (for
  (z 0 500 20)
  ((if not-eka draw point)
   (+ x z)
   (ope y (f z)))
  (setq not-eka t)))

(defun kulma-x (x) (any-rima x (- WIDTH 5) LENTGH))

(defun reuna-y
 (x)
 (setq x (/ (- x 250) 10))
 (+ (/ (* x x) 70) 12))

(defun piirra ()
 (setq WIDTH 60)
 (repeat
  (display-mode 6)
  (setq LENTGH 500)
  (mapc
   '(reuna-x kansi1-x kansi2-x)
   (function (lambda (v) (rima - 0 60 (eval v)))))
  (mapc
   '(reuna-x kulma-x koli-x pohja-x)
   (function (lambda (v) (rima + 0 60 (eval v)))))
  (mapc
   '(reuna-y kulma-y koli-y pohja-y kansi1-y kansi2-y)
   (function (lambda (v) (rima - 50 30 (eval v)))))
  (for (z 0 250 20) (kaari z 150 195 4))
  (for
   (z 0 500 20)
   (kaari z
    (+ 450 (/ z 5))
    (- 200 (/ z 5))
    3))
  (for
   (z 0 500 30)
   (kaari
    (- 500 z)
    (+ 300 (/ z 10))
    (- 200 (/ z 10))
    2))
  (g-char-mode 1)
  (point 550 5)
  (print (list 'leveys WIDTH))
  (point 550 13)
  (print (list 'pituus LENTGH))
  (g-char-mode 0)
  (case
   (readcc)
   (43 (setq WIDTH (+ WIDTH 5)) nil)
   (45 (setq WIDTH (- WIDTH 5)) nil)
   (t t))))

(defun reuna-x (z) (any-rima z WIDTH LENTGH))

(defq kano
 (fib stereo any-rima solve-x^2 iso-kuva fast kansi2-y kansi2-x
  kansi1-y kansi1-x pohja-y pohja-x kaari koli-y koli-x kulma-y
  rima kulma-x reuna-y piirra reuna-x kano))
