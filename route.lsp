
'(%Z%%M% %R%.%L%.%B%.%S% %E% %Y%)
'(MIKKO-3 (10 / 6 - 1994) (12 : 31 : 24 65))
(defq *package* ROUTE)

(defun find-bus
 (c)
 (while c
  (if
   (setq tmp (cadr-assoc (caar c) c))
   (nconc tmp (list (cadr (pop c)))))))

(defq connections
 ((1 10)
  (1 5)
  (1 19)
  (3 6)
  (4 10)
  (11 15)
  (5 13)))

(defun remo-self
 (c d)
 (filter c
  (function
   (lambda (x) (not (equal (car x) (cadr x)))))))

(defun cadr-assoc
 (x c)
 (if c
  (if
   (equal (cadr (car c)) x)
   (car c)
   (cadr-assoc x (cdr c)))))

(defun remo-doub
 (c d)
 (setq more t)
 (while more
  (setq d nil)
  (setq more nil)
  (while c
   (while
    (setq tmp (cadr-assoc (cadr (car c)) (cdr c)))
    (push-once (list (car (pop c)) (car tmp)) c)
    (setq more t))
   (while
    (setq tmp (assoc (caar c) (cdr c)))
    (push-once (list (cadr (pop c)) (cadr tmp)) c)
    (setq more t))
   (if
    (and c (not (equal (caar c) (cadr (car c)))))
    (push (pop c) d)))
  (setq c (reverse (sort-size d))))
 c)

(defun draw-conn
 (c)
 (display-mode 6)
 (setq y 180)
 (while c
  (let
   ((a (* 10 (caar c)))
    (b (* 10 (cadr (pop c)))))
   (point a 0)
   (draw a y)
   (draw b y)
   (draw b 0)
   (setq y (- y 7))))
 (readcc)
 (display-mode 3))

(defun sort-size
 (connections)
 (map
  (map
   (sort
    (map connections
     (function
      (lambda
       (x)
       (cons (abs (- (car x) (cadr x))) x)))))
   cdr)
  sort))

(defun random
 (m)
 (setq *PREV*
  (peek (+ 5 (or *PREV* 56)) (car (last (time)))))
 (remainder (if (zerop *PREV*) (random m) *PREV*) m))

(defun gen-pins
 (connections)
 (unless connections
  (for
   (x 1 40)
   (push-once (list (random 40) (random 40)) connections)))
 (draw-conn connections)
 (setq connections (remo-doub (remo-doub (remo-self (reverse (sort-size connections))))))
 (draw-conn connections)
 connections)

(defq ROUTE (find-bus connections remo-self cadr-assoc remo-doub draw-conn sort-size random gen-pins ROUTE))
