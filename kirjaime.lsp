
'(%Z%%M% %R%.%L%.%B%.%S% %E% %Y%)
'(MIKKO-3 (30 / 6 - 1994) (9 : 24 : 7 61))
(defq *package* KIRJAIME)

(defun russian
 (x)
 (cond
  ((identp x) (compress (map (explode x) ruski)))
  ((atom x) x)
  (t (cons (russian (car x)) (russian (cdr x))))))

(defun ruski
 (ch)
 (if (> ch 96) (setq ch (- ch 32)))
 (case ch
  (65 (char �))
  (66 (char �))
  (86 (char �))
  (71 (char �))
  (68 (char �))
  (69 (char �))
  (73 (char �))
  (75 (char �))
  (76 (char �))
  (77 (char �))
  (78 (char �))
  (79 (char �))
  (80 (char �))
  (82 (char �))
  (83 (char �))
  (84 (char �))
  (85 (char �))
  (70 (char �))
  (72 (char �))
  (90 (char �))
  (89 (char �))
  (t ch)))

(defun kirj ()
 (for (x 0 15) (tab (* 3 x)) (print x))
 (cr)
 (for
  (x 128 255)
  (if (zerop (remainder x 16)) (cr))
  (tab (* 3 (remainder x 16)))
  (printc x))
 (cr))

(defq KIRJAIME (russian ruski kirj KIRJAIME))
