
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
  (65 (char Ä))
  (66 (char Å))
  (86 (char Ç))
  (71 (char É))
  (68 (char Ñ))
  (69 (char Ö))
  (73 (char à))
  (75 (char ä))
  (76 (char ã))
  (77 (char å))
  (78 (char ç))
  (79 (char é))
  (80 (char è))
  (82 (char ê))
  (83 (char ë))
  (84 (char í))
  (85 (char ì))
  (70 (char î))
  (72 (char ï))
  (90 (char ñ))
  (89 (char õ))
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
